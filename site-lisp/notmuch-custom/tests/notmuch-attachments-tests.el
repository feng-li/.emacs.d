;;; notmuch-attachments-tests.el --- Thread export UI checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'notmuch-custom-attachments)

(ert-deftest notmuch-attachments-requires-opened-thread ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'read-directory-name)
               (lambda (&rest _) (ert-fail "Must reject the mode before prompting"))))
      (should-error (call-interactively #'notmuch-custom-extract-thread-attachments)
                    :type 'user-error))))

(ert-deftest notmuch-attachments-rejects-remote-destination ()
  (with-temp-buffer
    (setq major-mode 'notmuch-show-mode)
    (let ((notmuch-custom-attachments--process nil))
      (should-error (notmuch-custom-extract-thread-attachments "/ssh:example:/tmp/export")
                    :type 'user-error))))

(ert-deftest notmuch-attachments-rejects-file-destination ()
  (let ((file (make-temp-file "notmuch-export-file-")))
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'notmuch-show-mode)
          (let ((notmuch-custom-attachments--process nil))
            (should-error (notmuch-custom-extract-thread-attachments file) :type 'user-error)))
      (delete-file file))))

(ert-deftest notmuch-attachments-rejects-second-running-export ()
  (let* ((process (make-pipe-process :name "attachment-busy-test" :noquery t))
         (notmuch-custom-attachments--process process))
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'notmuch-show-mode)
          (should-error (notmuch-custom-extract-thread-attachments temporary-file-directory)
                        :type 'user-error))
      (delete-process process))))

(ert-deftest notmuch-attachments-starts-worker-with-current-id-not-search-query ()
  (let* ((process (make-pipe-process :name "attachment-payload-test" :noquery t))
         (notmuch-custom-attachments--process nil)
         command payload eof)
    (unwind-protect
        (with-temp-buffer
          (setq major-mode 'notmuch-show-mode)
          (setq-local notmuch-show-query-context "tag:inbox")
          (setq-local notmuch-show-thread-id "thread:unrelated-context")
          (cl-letf (((symbol-function 'notmuch-show-get-message-id)
                     (lambda (&optional bare) (should bare) "selected@example.test"))
                    ((symbol-function 'executable-find) (lambda (name) (concat "/bin/" name)))
                    ((symbol-function 'make-process)
                     (lambda (&rest args) (setq command (plist-get args :command)) process))
                    ((symbol-function 'process-send-string)
                     (lambda (actual text) (should (eq actual process)) (setq payload text)))
                    ((symbol-function 'process-send-eof) (lambda (_) (setq eof t)))
                    ((symbol-function 'display-buffer) #'ignore))
            (should (eq (notmuch-custom-extract-thread-attachments temporary-file-directory) process))
            (should eof)
            (should (equal (cadr command) "-u"))
            (should (string-suffix-p "/notmuch-custom/notmuch-attachments.py" (nth 2 command)))
            (let* ((json-object-type 'plist)
                   (job (json-read-from-string payload)))
              (should (equal (plist-get job :message_query) "id:selected@example.test"))
              (should (equal (plist-get job :directory) (expand-file-name temporary-file-directory)))
              (should (equal (plist-get job :notmuch_command) "/bin/notmuch")))))
      (delete-process process)
      (when (get-buffer "*notmuch-attachments*") (kill-buffer "*notmuch-attachments*")))))

(defun notmuch-attachments-test--finish (json status)
  "Run the exit callback with JSON and STATUS without starting a worker."
  (let* ((buffer (generate-new-buffer " *attachment-result-test*"))
         (process (make-pipe-process :name "attachment-result-test" :buffer buffer :noquery t))
         (notmuch-custom-attachments--process process)
         notification)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "Progress\nNOTMUCH-ATTACHMENTS-RESULT " json "\n"))
          (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_) status))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (setq notification (apply #'format format-string args)))))
            (notmuch-custom-attachments--finished process "finished\n"))
          (should-not notmuch-custom-attachments--process)
          notification)
      (delete-process process)
      (kill-buffer buffer))))

(ert-deftest notmuch-attachments-reports-success-and-index ()
  (let ((text (notmuch-attachments-test--finish
               "{\"ok\":true,\"saved\":2,\"errors\":0,\"messages\":3,\"manifest\":\"/tmp/index.jsonl\"}" 0)))
    (should (string-match-p "Saved 2 attachment(s) from 3 message(s)" text))
    (should (string-match-p "/tmp/index.jsonl" text))))

(ert-deftest notmuch-attachments-reports-empty-thread ()
  (should
   (string-match-p
    "no attachments found"
    (notmuch-attachments-test--finish
     "{\"ok\":true,\"saved\":0,\"errors\":0,\"messages\":1,\"manifest\":null}" 0))))

(ert-deftest notmuch-attachments-reports-partial-failure ()
  (should
   (string-match-p
    "incomplete: 2 saved, 1 failed"
    (notmuch-attachments-test--finish
     "{\"ok\":false,\"saved\":2,\"errors\":1,\"messages\":3,\"manifest\":\"/tmp/index.jsonl\"}" 1))))

(ert-deftest notmuch-attachments-reports-worker-error ()
  (should
   (string-match-p
    "incomplete: cannot read thread"
    (notmuch-attachments-test--finish "{\"ok\":false,\"error\":\"cannot read thread\"}" 1))))

(ert-deftest notmuch-attachments-real-worker-and-repeat-export ()
  "Exercise Emacs, the Python worker, and result parsing with a fake indexer."
  (skip-unless (executable-find "python3"))
  (let* ((directory (make-temp-file "notmuch-attachment-worker-" t))
         (notmuch-command (expand-file-name "fake notmuch" directory))
         (output (expand-file-name "exports" directory))
         (notmuch-custom-attachments--process nil)
         (notmuch-custom-attachments-python-command "python3"))
    (unwind-protect
        (progn
          (with-temp-file notmuch-command
            (insert "#!/bin/sh\ncase \"$1 $2\" in\n"
                    "'search --format=json') printf '%s\\n' '[\"abc123\"]';;\n"
                    "'show --format=json') printf '%s\\n' "
                    (shell-quote-argument
                     "[[[{\"id\":\"selected@example.test\",\"headers\":{\"Subject\":\"Test\"},\"body\":[{\"id\":2,\"content-type\":\"application/octet-stream\",\"content-disposition\":\"attachment\",\"filename\":\"data.bin\"}]},[]]]]")
                    ";;\n'show --format=raw') printf '\\000\\377TEST\\r\\n';;\n"
                    "*) exit 19;;\nesac\n"))
          (set-file-modes notmuch-command #o700)
          (dotimes (_ 2)
            (with-temp-buffer
              (setq major-mode 'notmuch-show-mode)
              (cl-letf (((symbol-function 'notmuch-show-get-message-id)
                         (lambda (&optional _) "selected@example.test"))
                        ((symbol-function 'display-buffer) #'ignore))
                (let ((process (notmuch-custom-extract-thread-attachments output))
                      (deadline (+ (float-time) 15)))
                  (while (and notmuch-custom-attachments--process (< (float-time) deadline))
                    (accept-process-output nil 0.05))
                  (should-not notmuch-custom-attachments--process)
                  (should (= (process-exit-status process) 0))
                  (with-current-buffer (process-buffer process)
                    (should buffer-read-only)
                    (should (string-match-p "NOTMUCH-ATTACHMENTS-RESULT " (buffer-string))))))))
          (dolist (name '("data.bin" "data (2).bin"))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally (expand-file-name name output))
              (should (equal (buffer-string) (unibyte-string 0 255 84 69 83 84 13 10)))))
          (dolist (name '("attachments.jsonl" "attachments (2).jsonl"))
            (with-temp-buffer
              (insert-file-contents (expand-file-name name output))
              (let* ((json-object-type 'plist)
                     (row (json-read-from-string (buffer-string))))
                (should (equal (plist-get row :message_id) "selected@example.test"))
                (should (equal (plist-get row :status) "saved"))))))
      (when (and notmuch-custom-attachments--process
                 (process-live-p notmuch-custom-attachments--process))
        (delete-process notmuch-custom-attachments--process))
      (when (get-buffer "*notmuch-attachments*") (kill-buffer "*notmuch-attachments*"))
      (delete-directory directory t))))

;;; notmuch-attachments-tests.el ends here
