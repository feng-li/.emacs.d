;;; notmuch-custom-async-test.el --- Background sync tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../notmuch-custom.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(ert-deftest notmuch-custom-async-worker-keeps-indexing-on-star-failure ()
  (let ((notmuch-custom-imap-starred-sync t) polled)
    (cl-letf (((symbol-function 'notmuch-custom--poll-with-senders)
               (lambda (&rest _) (setq polled t)))
              ((symbol-function 'notmuch-custom-sync-imap-starred)
               (lambda () (error "Offline"))))
      (let ((result (notmuch-custom--sync-worker)))
        (should polled)
        (should (plist-get result :ok))
        (should (equal (plist-get result :star-error) "Offline"))))))

(defun notmuch-custom-async-test--run (fail)
  "Run a real child Emacs against an isolated fake indexer, optionally FAIL."
  (let* ((directory (make-temp-file "notmuch-async-test-" t))
         (script (expand-file-name "notmuch" directory))
         (notmuch-command script)
         (notmuch-poll-script nil)
         (notmuch-custom-imap-starred-sync nil)
         (notmuch-custom--sync-process nil)
         (auth-source-xoauth2-plugin-mode nil)
         (refreshes 0) heartbeat timer process)
    (unwind-protect
        (progn
          (with-temp-file script
            (insert "#!/bin/sh\ncase \"$1\" in\n"
                    "new) sleep 0.3; touch \"$0.done\"; exit " (if fail "1" "0") ";;\n"
                    "search) if test -f \"$0.done\"; then printf '(\"id:new@example.org\")\\n'; else printf '()\\n'; fi;;\n"
                    "address) printf '((:name \"Test Sender\" :address \"sender@example.org\"))\\n';;\nesac\n"))
          (set-file-modes script #o700)
          (cl-letf (((symbol-function 'notmuch-refresh-all-buffers)
                     (lambda () (cl-incf refreshes))))
            (setq process (notmuch-custom-poll-and-refresh))
            ;; Starting a second sync must reuse the running worker.
            (should (eq process (notmuch-custom-poll-and-refresh)))
            (should (= refreshes 0))
            (setq timer (run-at-time 0.05 nil
                                     (lambda ()
                                       (setq heartbeat (process-live-p process)))))
            (let ((deadline (+ (float-time) 15)))
              (while (and notmuch-custom--sync-process (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should heartbeat)
            (should-not notmuch-custom--sync-process)
            (should (= refreshes (if fail 0 1)))
            (with-current-buffer (process-buffer process)
              (should (string-match-p "NOTMUCH-SYNC-RESULT " (buffer-string)))
              (unless fail
                (should (string-match-p "New mail from Test Sender" (buffer-string)))))))
      (when timer (cancel-timer timer))
      (when (and process (process-live-p process)) (delete-process process))
      (delete-directory directory t))))

(ert-deftest notmuch-custom-async-success-refreshes-after-exit ()
  (notmuch-custom-async-test--run nil))

(ert-deftest notmuch-custom-async-index-failure-does-not-refresh ()
  (notmuch-custom-async-test--run t))

;;; notmuch-custom-async-test.el ends here
