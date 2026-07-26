;;; latexmkpvc.el --- Continuous latexmk builds for AUCTeX  -*- lexical-binding: t; -*-

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (auctex "14.0"))
;; Keywords: tex, tools
;; URL: https://github.com/feng-li/.emacs.d

;;; Commentary:

;; This package adds a latexmk command using the `-pvc' option to AUCTeX.
;; Unlike an ordinary TeX command, a `latexmk -pvc' process stays alive and
;; rebuilds the document whenever an input file changes.  `latexmkpvc-mode'
;; adapts AUCTeX to that long-running process by:
;;
;; - keeping only the latest rebuild cycle in the TeX output buffer;
;; - parsing errors as output arrives instead of waiting for process exit; and
;; - displaying the output and visiting the first error after a failed cycle.
;;
;; Enable the integration in one AUCTeX LaTeX buffer with:
;;
;;   (latexmkpvc-mode 1)
;;
;; Or enable it for all AUCTeX LaTeX buffers with:
;;
;;   (latexmkpvc-setup)

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'tex)

(defgroup latexmkpvc nil
  "Continuous latexmk builds in AUCTeX."
  :group 'TeX-command
  :prefix "latexmkpvc-")

(defcustom latexmkpvc-command-name "LaTeXMkPvc"
  "Name of the continuous latexmk entry in `TeX-command-list'."
  :type 'string
  :group 'latexmkpvc)

(defcustom latexmkpvc-command
  "latexmk -gg -pvc %(latexmk-out) %(file-line-error) %`%(extraopts) %S%(mode)%' %t"
  "AUCTeX command template used for continuous latexmk builds.

The AUCTeX expansion forms in the default value select the engine, enable
file-and-line diagnostics, and preserve `TeX-command-extra-options'."
  :type 'string
  :group 'latexmkpvc)

(defcustom latexmkpvc-set-default-command t
  "Whether `latexmkpvc-mode' makes its command the AUCTeX default."
  :type 'boolean
  :group 'latexmkpvc)

(defcustom latexmkpvc-clear-output-on-rerun t
  "Whether to discard output from the preceding latexmk rebuild cycle."
  :type 'boolean
  :group 'latexmkpvc)

(defcustom latexmkpvc-show-compilation nil
  "Whether to display the output buffer when a continuous build starts.

When nil, the output buffer remains hidden until latexmk reports a definitive
error, unless the user displays it explicitly."
  :type 'boolean
  :group 'latexmkpvc)

(defcustom latexmkpvc-show-output-on-error t
  "Whether to display the TeX output after a failed latexmk cycle."
  :type 'boolean
  :group 'latexmkpvc)

(defcustom latexmkpvc-jump-to-error t
  "Whether to visit the first parseable error after a failed latexmk cycle."
  :type 'boolean
  :group 'latexmkpvc)

(defcustom latexmkpvc-error-display-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . 0.25)
    (preserve-size . (nil . t)))
  "Display action used for the TeX output buffer after an error.

The value is passed as the ACTION argument of `display-buffer'.  Set it to nil
to use `display-buffer-alist' and the standard display behavior."
  :type '(choice (const :tag "Use standard display behavior" nil)
                 (sexp :tag "Display action"))
  :group 'latexmkpvc)

(defconst latexmkpvc--cycle-regexp
  (regexp-opt
   '("Latexmk: Need to remake files."
     "Latexmk: New file(s) found."))
  "Regexp matching the beginning of a latexmk rebuild cycle.")

(defconst latexmkpvc--error-regexp
  (regexp-opt
   '("Latexmk: Errors, so I did not complete making targets"
     "Latexmk: Failure in processing file"
     "==> You will need to change a source file before I do another run <=="))
  "Regexp matching latexmk's definitive failure messages.")

(defconst latexmkpvc--output-tail-length 512
  "Number of process-output characters retained across filter calls.")

(defvar-local latexmkpvc--configured-p nil)
(defvar-local latexmkpvc--saved-command-default nil)
(defvar-local latexmkpvc--saved-command-list nil)
(defvar-local latexmkpvc--saved-command-list-local-p nil)

(defun latexmkpvc--last-match (regexp string)
  "Return bounds of the last match for REGEXP in STRING.

The return value is a cons cell of the match beginning and end, or nil when
REGEXP does not match."
  (let ((search-start 0)
        bounds)
    (while (string-match regexp string search-start)
      (setq bounds (cons (match-beginning 0) (match-end 0))
            search-start (match-end 0)))
    bounds))

(defun latexmkpvc--clear-output-buffer (process)
  "Clear PROCESS output while retaining AUCTeX's command header."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((header
               (save-excursion
                 (goto-char (point-min))
                 (when (looking-at "Running `")
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-beginning-position 2)))))
              (inhibit-read-only t))
          (erase-buffer)
          (when header
            (insert header))
          (set-marker (process-mark process) (point-max))
          (TeX-parse-reset)
          (when (bound-and-true-p compilation-minor-mode)
            (compilation-forget-errors)))))))

(defun latexmkpvc--jump-to-first-error (buffer)
  "Visit the first parseable error in the latest cycle of BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless (bound-and-true-p compilation-minor-mode)
        (compilation-minor-mode 1))
      (save-restriction
        (widen)
        (goto-char (point-max))
        (let ((start
               (if (re-search-backward latexmkpvc--cycle-regexp nil t)
                   (match-end 0)
                 (point-min)))
              ;; Ignore warnings and informational diagnostics.
              (compilation-skip-threshold 2))
          (setq compilation-current-error (copy-marker start))
          (condition-case error-data
              (compilation-next-error-function 1)
            (user-error
             (message "No parseable file:line LaTeX error found"))
            (error
             (message "Could not jump to LaTeX error: %s"
                      (error-message-string error-data)))))))))

(defun latexmkpvc--report-error (process)
  "Report a failed latexmk rebuild handled by PROCESS."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (when latexmkpvc-show-output-on-error
        (let ((window (display-buffer buffer latexmkpvc-error-display-action)))
          (when (window-live-p window)
            (set-window-point
             window
             (with-current-buffer buffer (point-max))))))
      (when latexmkpvc-jump-to-error
        (run-at-time 0 nil #'latexmkpvc--jump-to-first-error buffer)))))

(defun latexmkpvc--process-filter (process output)
  "Handle continuous latexmk OUTPUT from PROCESS.

The original AUCTeX process filter is called after any obsolete rebuild output
has been removed."
  (when (buffer-live-p (process-buffer process))
    (let* ((tail (or (process-get process 'latexmkpvc--output-tail) ""))
           (text (concat tail output))
           (cycle (latexmkpvc--last-match latexmkpvc--cycle-regexp text))
           (scan text)
           (original-filter
            (or (process-get process 'latexmkpvc--original-filter)
                #'TeX-format-filter)))
      (when cycle
        (process-put process 'latexmkpvc--error-shown nil)
        (setq scan (substring text (cdr cycle)))
        (when latexmkpvc-clear-output-on-rerun
          (latexmkpvc--clear-output-buffer process)
          ;; Reinsert a complete marker when it was split across chunks.
          (setq output (substring text (car cycle)))))
      (funcall original-filter process output)
      (when (and (not (process-get process 'latexmkpvc--error-shown))
                 (string-match-p latexmkpvc--error-regexp scan))
        (process-put process 'latexmkpvc--error-shown t)
        (latexmkpvc--report-error process))
      (process-put
       process 'latexmkpvc--output-tail
       (substring scan
                  (max 0 (- (length scan)
                            latexmkpvc--output-tail-length)))))))

(defun latexmkpvc-run (name command file)
  "Run continuous latexmk command NAME using COMMAND on FILE.

This function has the command-runner signature required by
`TeX-command-list'."
  (unless TeX-process-asynchronous
    (user-error "Continuous latexmk builds require `TeX-process-asynchronous'"))
  (let* ((TeX-show-compilation latexmkpvc-show-compilation)
         (process (TeX-run-TeX name command file)))
    (when (processp process)
      (process-put process 'latexmkpvc--original-filter
                   (process-filter process))
      (process-put process 'latexmkpvc--output-tail "")
      (process-put process 'latexmkpvc--error-shown nil)
      (set-process-filter process #'latexmkpvc--process-filter)
      (with-current-buffer (process-buffer process)
        (unless (bound-and-true-p compilation-minor-mode)
          (compilation-minor-mode 1))))
    process))

(defun latexmkpvc--command-entry ()
  "Return the AUCTeX command entry for `latexmkpvc-mode'."
  (list latexmkpvc-command-name
        latexmkpvc-command
        #'latexmkpvc-run
        nil
        '(LaTeX-mode docTeX-mode)
        :help "Clean and rebuild continuously with latexmk (-gg -pvc)"))

(defun latexmkpvc--enable ()
  "Install the continuous latexmk command in the current buffer."
  (unless latexmkpvc--configured-p
    (setq latexmkpvc--saved-command-default TeX-command-default
          latexmkpvc--saved-command-list TeX-command-list
          latexmkpvc--saved-command-list-local-p
          (local-variable-p 'TeX-command-list)
          latexmkpvc--configured-p t)
    (setq-local
     TeX-command-list
     (cons
      (latexmkpvc--command-entry)
      (cl-remove-if
       (lambda (entry)
         (and (consp entry)
              (stringp (car entry))
              (string= (car entry) latexmkpvc-command-name)))
       TeX-command-list)))
    (when latexmkpvc-set-default-command
      (setq TeX-command-default latexmkpvc-command-name))))

(defun latexmkpvc--disable ()
  "Remove the continuous latexmk command from the current buffer."
  (when latexmkpvc--configured-p
    (if latexmkpvc--saved-command-list-local-p
        (setq-local TeX-command-list latexmkpvc--saved-command-list)
      (kill-local-variable 'TeX-command-list))
    (setq TeX-command-default latexmkpvc--saved-command-default
          latexmkpvc--configured-p nil
          latexmkpvc--saved-command-default nil
          latexmkpvc--saved-command-list nil
          latexmkpvc--saved-command-list-local-p nil)))

;;;###autoload
(define-minor-mode latexmkpvc-mode
  "Use a continuous latexmk process in the current AUCTeX LaTeX buffer."
  :init-value nil
  :lighter nil
  :keymap nil
  :group 'latexmkpvc
  (if latexmkpvc-mode
      (if (derived-mode-p 'LaTeX-mode 'docTeX-mode)
          (latexmkpvc--enable)
        (setq latexmkpvc-mode nil)
        (user-error "latexmkpvc-mode only supports AUCTeX LaTeX buffers"))
    (latexmkpvc--disable)))

(defun latexmkpvc--turn-on ()
  "Enable `latexmkpvc-mode' in an AUCTeX LaTeX buffer."
  (when (derived-mode-p 'LaTeX-mode 'docTeX-mode)
    (latexmkpvc-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-latexmkpvc-mode
  latexmkpvc-mode latexmkpvc--turn-on
  :group 'latexmkpvc)

;;;###autoload
(defun latexmkpvc-setup ()
  "Enable `latexmkpvc-mode' in all AUCTeX LaTeX buffers."
  (interactive)
  (global-latexmkpvc-mode 1))

(provide 'latexmkpvc)

;;; latexmkpvc.el ends here
