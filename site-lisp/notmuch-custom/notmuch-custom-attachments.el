;;; notmuch-custom-attachments.el --- Export an opened thread -*- lexical-binding: t; -*-

;;; Commentary:
;; Background, read-only attachment export for the thread containing point.

;;; Code:

(require 'json)
(require 'notmuch-lib)
(require 'notmuch-show)
(require 'subr-x)

(defgroup notmuch-custom-attachments nil
  "Batch attachment export from opened Notmuch threads."
  :group 'notmuch)

(defcustom notmuch-custom-attachments-python-command "python3"
  "Python 3 executable used by the attachment exporter.
The exporter uses the standard library; tqdm is optional."
  :type 'string
  :group 'notmuch-custom-attachments)

(defconst notmuch-custom-attachments--script
  (expand-file-name "notmuch-attachments.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Attachment exporter next to this library, including when byte-compiled.")

(defvar notmuch-custom-attachments--process nil
  "Currently running attachment exporter, or nil.")

(defun notmuch-custom-attachments--finished (process _event)
  "Report the result when attachment exporter PROCESS exits."
  (when (memq (process-status process) '(exit signal))
    (when (eq process notmuch-custom-attachments--process)
      (setq notmuch-custom-attachments--process nil))
    (let ((buffer (process-buffer process)) result)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-max))
            (when (re-search-backward "^NOTMUCH-ATTACHMENTS-RESULT \\(.+\\)$" nil t)
              (let ((json-object-type 'plist)
                    (json-array-type 'list)
                    (json-false nil))
                (setq result
                      (ignore-errors (json-read-from-string (match-string 1)))))))
          (special-mode)))
      (cond
       ((and result (zerop (process-exit-status process)) (plist-get result :ok))
        (message "Saved %d attachment(s) from %d message(s)%s"
                 (plist-get result :saved) (plist-get result :messages)
                 (if-let* ((manifest (plist-get result :manifest)))
                     (concat "; index: " manifest)
                   "; no attachments found")))
       (t
        (message "Attachment export incomplete: %s (see *notmuch-attachments*)"
                 (or (plist-get result :error)
                     (and result (format "%d saved, %d failed"
                                         (or (plist-get result :saved) 0)
                                         (or (plist-get result :errors) 0)))
                     (process-status process))))))))

;;;###autoload
(defun notmuch-custom-extract-thread-attachments (directory)
  "Export all attachments from the opened thread containing point to DIRECTORY.
Include collapsed and non-matching messages in that thread, not the entire
search that opened it.  Prompt for one local directory, then run in the
background; progress and errors appear in `*notmuch-attachments*'.

Existing files are never overwritten.  Named inline files are included;
unnamed inline bodies are not.  A JSONL index records each attachment's
message headers and saved filename.  Messages and tags are left unchanged."
  (interactive
   (progn
     (unless (derived-mode-p 'notmuch-show-mode)
       (user-error "Open a Notmuch thread before extracting attachments"))
     (list (read-directory-name "Extract thread attachments to: "
                                (expand-file-name "~/Downloads/")))))
  (unless (derived-mode-p 'notmuch-show-mode)
    (user-error "Open a Notmuch thread before extracting attachments"))
  (when (and notmuch-custom-attachments--process
             (process-live-p notmuch-custom-attachments--process))
    (user-error "An attachment export is already running; see *notmuch-attachments*"))
  (setq directory (expand-file-name directory))
  (when (file-remote-p directory)
    (user-error "Choose a local destination directory"))
  (when (and (file-exists-p directory) (not (file-directory-p directory)))
    (user-error "Destination is not a directory: %s" directory))
  (let* ((id (notmuch-show-get-message-id t))
         (python (executable-find notmuch-custom-attachments-python-command))
         (notmuch (executable-find notmuch-command))
         (default-directory (expand-file-name "~/"))
         (buffer (get-buffer-create "*notmuch-attachments*")))
    (unless (and (stringp id) (not (string-empty-p id)))
      (user-error "No message at point"))
    (unless python (user-error "Cannot find Python: %s" notmuch-custom-attachments-python-command))
    (unless notmuch (user-error "Cannot find notmuch: %s" notmuch-command))
    (unless (file-readable-p notmuch-custom-attachments--script)
      (user-error "Cannot find exporter: %s" notmuch-custom-attachments--script))
    (with-current-buffer buffer
      (fundamental-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Exporting the opened thread's attachments to " directory "\n\n")))
    (setq notmuch-custom-attachments--process
          (make-process :name "notmuch-attachments" :buffer buffer
                        :command (list python "-u" notmuch-custom-attachments--script)
                        :connection-type 'pipe :coding 'utf-8-unix :noquery t
                        :sentinel #'notmuch-custom-attachments--finished))
    (condition-case error-data
        (progn
          (process-send-string
           notmuch-custom-attachments--process
           (concat (json-encode `((message_query . ,(notmuch-id-to-query id))
                                  (directory . ,directory)
                                  (notmuch_command . ,notmuch))) "\n"))
          (process-send-eof notmuch-custom-attachments--process))
      (error
       (delete-process notmuch-custom-attachments--process)
       (setq notmuch-custom-attachments--process nil)
       (signal (car error-data) (cdr error-data))))
    (display-buffer buffer)
    (message "Thread attachment export started; see *notmuch-attachments*")
    notmuch-custom-attachments--process))

(provide 'notmuch-custom-attachments)
;;; notmuch-custom-attachments.el ends here
