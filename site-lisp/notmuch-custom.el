;;; notmuch-custom.el --- Local enhancements for Notmuch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (company "1.0.0") (ivy "0.15.0") (notmuch "0.38"))
;; Keywords: mail, completion, multilingual

;;; Commentary:

;; Local Notmuch enhancements:
;;
;; - saved searches generated from the active Notmuch profile;
;; - address completion using literal text, full Pinyin, or Pinyin initials;
;; - compact, expandable To and Cc headers;
;; - attachment opening through the desktop default application; and
;; - coexistence of address completion in headers and word completion in bodies.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ivy-pinyin-search)
(require 'mail-extr)
(require 'notmuch)
(require 'notmuch-address)
(require 'notmuch-company)
(require 'notmuch-show)
(require 'subr-x)

(defgroup notmuch-custom nil
  "Local enhancements for Notmuch."
  :group 'notmuch)

;;; Saved searches

(defun notmuch-custom-query-display-name (query-name)
  "Turn an ordered Notmuch QUERY-NAME into a display label.
The numeric prefix controls order, a hyphen becomes a space, and a
double hyphen becomes a folder separator."
  (let ((name (replace-regexp-in-string "\\`[0-9]+-" "" query-name)))
    (setq name (replace-regexp-in-string "--" " / " name))
    (replace-regexp-in-string "-" " " name)))

(defun notmuch-custom-saved-searches-from-profile ()
  "Build Emacs saved searches from the active profile's query.* entries."
  (let (query-names)
    (dolist (line (notmuch--process-lines notmuch-command "config" "list"))
      (when (string-match "\\`query\\.\\([^=]+\\)=" line)
        (push (match-string 1 line) query-names)))
    (mapcar (lambda (query-name)
              (list :name (notmuch-custom-query-display-name query-name)
                    :query (concat "query:" query-name)
                    :search-type 'unthreaded))
            (sort query-names #'string-lessp))))

;;; Pinyin address completion

(defvar-local notmuch-custom-company-pinyin--last-prefix nil)

(defun notmuch-custom--company-pinyin-regexp (input)
  "Build a Pinyin regexp for simple Latin INPUT.
Return nil when INPUT contains characters unsuitable for Pinyin matching."
  (when (string-match-p "\\`[A-Za-z \\t]+\\'" input)
    (let* ((text (downcase input))
           (full (ivy-pinyin-search--full-regexp text))
           (initials (ivy-pinyin-search--initial-regexp text t))
           (regexps (delete-dups (delq nil (list full initials)))))
      (cond
       ((null regexps) nil)
       ((null (cdr regexps)) (car regexps))
       (t (concat "\\(?:" (mapconcat #'identity regexps "\\|") "\\)"))))))

(defun notmuch-custom--company-pinyin-matching (input)
  "Return cached Notmuch addresses matching literal or Pinyin INPUT."
  (let ((case-fold-search t)
        (literal-regexp (regexp-quote input))
        (pinyin-regexp (notmuch-custom--company-pinyin-regexp input))
        candidates)
    (maphash
     (lambda (candidate _value)
       (when (or (string-match-p literal-regexp candidate)
                 (and pinyin-regexp
                      (string-match-p pinyin-regexp candidate)))
         (push candidate candidates)))
     notmuch-address-completions)
    candidates))

(defun notmuch-custom--company-pinyin-harvest (input callback buffer)
  "Harvest all addresses, then call CALLBACK with matches for INPUT in BUFFER."
  (setq notmuch-address-last-harvest (float-time))
  (notmuch-address-harvest
   nil nil
   (lambda (_process event)
     (let ((finished (string= event "finished\n")))
       (if finished
           (progn
             (setq notmuch-address-full-harvest-finished t)
             (notmuch-address--save-address-hash))
         (setq notmuch-address-last-harvest 0))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (funcall callback
                    (and finished
                         (notmuch-custom--company-pinyin-matching input)))))))))

(defun notmuch-custom-company-pinyin (command &optional arg &rest ignored)
  "Complete Notmuch addresses using literal text or Pinyin.
COMMAND, ARG, and IGNORED follow the Company backend protocol."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'notmuch-custom-company-pinyin))
    (prefix
     (setq notmuch-custom-company-pinyin--last-prefix
           (notmuch-company 'prefix)))
    (candidates
     (if (not (eq notmuch-address-command 'internal))
         (apply #'notmuch-company command arg ignored)
       (if (notmuch-address--harvest-ready)
           (progn
             (notmuch-address-harvest-trigger)
             (notmuch-custom--company-pinyin-matching arg))
         (let ((buffer (current-buffer)))
           (cons :async
                 (lambda (callback)
                   (notmuch-custom--company-pinyin-harvest
                    arg callback buffer)))))))
    (match
     ;; Pinyin input has no literal span to highlight in a Chinese name.
     (if (and notmuch-custom-company-pinyin--last-prefix
              (string-match notmuch-custom-company-pinyin--last-prefix arg))
         (match-end 0)
       0))
    (post-completion
     (run-hook-with-args 'notmuch-address-post-completion-functions arg))
    (ignore-case t)
    (no-cache t)))

(defun notmuch-custom-company-setup ()
  "Configure Company for addresses in headers and words in message bodies."
  (setq-local company-backends
              '(notmuch-custom-company-pinyin
                (company-yasnippet company-dabbrev company-ispell :separate)
                company-files)))

;;; Compact address headers

(defcustom notmuch-custom-address-header-limit 4
  "Maximum number of addresses displayed in a Notmuch To or Cc header."
  :type 'integer
  :group 'notmuch-custom)

(defun notmuch-custom--format-address (components)
  "Format parsed mail address COMPONENTS for display."
  (pcase-let ((`(,name ,address) components))
    (cond
     ((and name address) (format "%s <%s>" name address))
     (address address)
     (name name)
     (t ""))))

(defun notmuch-custom--insert-collapsed-address-button (addresses count)
  "Insert a button representing COUNT omitted ADDRESSES."
  (insert-text-button
   (format "… (%d more)" count)
   'action #'notmuch-custom--expand-address-header
   'follow-link t
   'help-echo "Show all recipients"
   'notmuch-custom-omitted-count count
   'notmuch-custom-omitted-addresses addresses))

(defun notmuch-custom--expand-address-header (button)
  "Replace the truncation BUTTON with its omitted addresses and a hide button."
  (let ((start (button-start button))
        (end (button-end button))
        (addresses (button-get button 'notmuch-custom-omitted-addresses))
        (count (button-get button 'notmuch-custom-omitted-count))
        (inhibit-read-only t))
    (goto-char start)
    (delete-region start end)
    (let ((expanded-start (copy-marker start)))
      (insert addresses " ")
      (insert-text-button
       "[hide extra addresses]"
       'action #'notmuch-custom--collapse-address-header
       'follow-link t
       'help-echo "Hide extra recipients"
       'notmuch-custom-expanded-start expanded-start
       'notmuch-custom-omitted-count count
       'notmuch-custom-omitted-addresses addresses))))

(defun notmuch-custom--collapse-address-header (button)
  "Collapse the extra addresses preceding BUTTON."
  (let* ((marker (button-get button 'notmuch-custom-expanded-start))
         (start (marker-position marker))
         (end (button-end button))
         (addresses (button-get button 'notmuch-custom-omitted-addresses))
         (count (button-get button 'notmuch-custom-omitted-count))
         (inhibit-read-only t))
    (set-marker marker nil)
    (delete-region start end)
    (goto-char start)
    (notmuch-custom--insert-collapsed-address-button addresses count)))

(defun notmuch-custom--insert-truncated-address-header
    (original-function header header-value)
  "Call ORIGINAL-FUNCTION, abbreviating a long address HEADER-VALUE."
  (let ((addresses
         (when (member header '("To" "Cc"))
           (condition-case nil
               (mail-extract-address-components header-value t)
             (error nil)))))
    (if (or (null addresses)
            (<= (length addresses) notmuch-custom-address-header-limit))
        (funcall original-function header header-value)
      (let* ((shown
              (cl-subseq addresses 0 notmuch-custom-address-header-limit))
             (omitted
              (nthcdr notmuch-custom-address-header-limit addresses))
             (shown-text
              (mapconcat #'notmuch-custom--format-address shown ", "))
             (omitted-text
              (mapconcat #'notmuch-custom--format-address omitted ", ")))
        (insert header ": " (notmuch-sanitize shown-text) ", ")
        (notmuch-custom--insert-collapsed-address-button
         (notmuch-sanitize omitted-text) (length omitted))
        (insert "\n")))))

;;; External attachment opening

(defun notmuch-custom-open-part-with-default-application ()
  "Open the MIME part at point using the desktop default application."
  (interactive)
  (unless (executable-find "xdg-open")
    (user-error "Cannot find xdg-open"))
  (notmuch-show-apply-to-current-part-handle
   (lambda (handle)
     (let* ((directory (make-temp-file "notmuch-attachment-" t))
            (original-name (or (mm-handle-filename handle) "attachment"))
            (base-name (file-name-nondirectory original-name))
            (file (expand-file-name
                   (if (string-empty-p base-name) "attachment" base-name)
                   directory))
            (process-connection-type nil))
       (mm-save-part-to-file handle file)
       (let ((process (start-process "notmuch-xdg-open" nil
                                     "xdg-open" file)))
         (set-process-query-on-exit-flag process nil))))))

;;; Setup

;;;###autoload
(defun notmuch-custom-setup ()
  "Enable the local Notmuch enhancements defined in this library."
  (unless (advice-member-p #'notmuch-custom--insert-truncated-address-header
                           'notmuch-show-insert-header)
    (advice-add 'notmuch-show-insert-header :around
                #'notmuch-custom--insert-truncated-address-header))
  (add-hook 'notmuch-message-mode-hook #'notmuch-custom-company-setup)
  (setq notmuch-show-part-button-default-action
        'notmuch-custom-open-part-with-default-application))

(provide 'notmuch-custom)

;;; notmuch-custom.el ends here
