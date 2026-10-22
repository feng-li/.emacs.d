;;; notmuch-msf-tests.el --- Local star sync checks -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../notmuch-custom.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t t)

(ert-deftest notmuch-msf-scope-and-known-unstarred ()
  (let (changes)
    (cl-letf (((symbol-function 'notmuch-custom--msf-files) (lambda () '("Inbox.msf")))
              ((symbol-function 'notmuch-custom--read-msf-files)
               (lambda (_) '(:starred ("new" "both") :unstarred ("old" "both"))))
              ((symbol-function 'notmuch-custom--imap-message-ids)
               (lambda (_) '("old" "both" "outside" "missing")))
              ((symbol-function 'notmuch-tag)
               (lambda (query tags &rest _) (push (list query tags) changes))))
      (notmuch-custom-sync-thunderbird-starred)
      (should (equal (sort changes (lambda (a b) (string< (car a) (car b))))
                     (list (list (notmuch-id-to-query "new") '("+imap-starred"))
                           (list (notmuch-id-to-query "old") '("-imap-starred"))))))))

(ert-deftest notmuch-msf-scan-failure-preserves-tags ()
  (cl-letf (((symbol-function 'notmuch-custom--msf-files) (lambda () '("missing.msf")))
            ((symbol-function 'notmuch-custom--read-msf-files) (lambda (_) (error "Incomplete")))
            ((symbol-function 'notmuch-tag) (lambda (&rest _) (ert-fail "Changed tags"))))
    (should-error (notmuch-custom-sync-thunderbird-starred))))

(ert-deftest notmuch-msf-nested-physical-paths ()
  (let ((notmuch-custom-imap-post-send-accounts '((:local-root "/mail"))))
    (cl-letf (((symbol-function 'notmuch-call-notmuch-sexp)
               (lambda (&rest _) '("/mail/Parent.sbd/Child/cur/1"
                                    "/mail/Parent.sbd/Child/new/2"
                                    "/other/INBOX/cur/3"))))
      (should (equal (notmuch-custom--msf-files) '("/mail/Parent.sbd/Child.msf"))))))

(ert-deftest notmuch-msf-command-routing ()
  (let (mode)
    (cl-letf (((symbol-function 'notmuch-custom--start-sync) (lambda (&optional m) (setq mode m))))
      (call-interactively #'notmuch-custom-sync-thunderbird-starred)
      (should (eq mode 'msf))
      (notmuch-custom-poll-and-refresh)
      (should-not mode))))

(ert-deftest notmuch-msf-auto-follows-indexing-and-preserves-notification ()
  (let ((notmuch-custom-imap-post-send-accounts '((:local-root "/mail"))) calls)
    (cl-letf (((symbol-function 'notmuch-custom--poll-with-senders)
               (lambda (&rest _)
                 (push 'index calls)
                 (setq notmuch-custom--sync-notification "New mail from Sender")))
              ((symbol-function 'notmuch-custom-sync-thunderbird-starred)
               (lambda () (push 'msf calls) "Local stars complete"))
              ((symbol-function 'notmuch-custom-sync-imap-starred)
               (lambda (&rest _) (ert-fail "Automatic IMAP scan"))))
      (let ((result (notmuch-custom--sync-worker)))
        (should (plist-get result :ok))
        (should (equal (reverse calls) '(index msf)))
        (should (equal (plist-get result :notification)
                       "New mail from Sender; Local stars complete"))))))

(ert-deftest notmuch-msf-auto-failure-keeps-indexing-success ()
  (let ((notmuch-custom-imap-post-send-accounts '((:local-root "/mail"))))
    (cl-letf (((symbol-function 'notmuch-custom--poll-with-senders) #'ignore)
              ((symbol-function 'notmuch-custom-sync-thunderbird-starred)
               (lambda () (error "Summary changed"))))
      (let ((result (notmuch-custom--sync-worker)))
        (should (plist-get result :ok))
        (should (equal (plist-get result :star-error) "Summary changed"))))))

(ert-deftest notmuch-msf-auto-index-failure-skips-star-scan ()
  (let ((notmuch-custom-imap-post-send-accounts '((:local-root "/mail"))))
    (cl-letf (((symbol-function 'notmuch-custom--poll-with-senders)
               (lambda (&rest _) (error "Index failed")))
              ((symbol-function 'notmuch-custom-sync-thunderbird-starred)
               (lambda () (ert-fail "Scanned after indexing failure"))))
      (should-not (plist-get (notmuch-custom--sync-worker) :ok)))))

(ert-deftest notmuch-msf-worker-does-not-poll-or-authenticate ()
  (cl-letf (((symbol-function 'notmuch-custom-sync-thunderbird-starred) (lambda () "Local complete"))
            ((symbol-function 'notmuch-custom-sync-imap-starred) (lambda (&rest _) (ert-fail "IMAP")))
            ((symbol-function 'notmuch-custom--poll-with-senders) (lambda (&rest _) (ert-fail "Indexing"))))
    (should (equal (plist-get (notmuch-custom--sync-worker 'msf) :notification) "Local complete"))))

(ert-deftest notmuch-msf-background-real-reader ()
  "Run the actual worker/reader with a fake indexer and isolated summary."
  (let* ((directory (make-temp-file "notmuch-msf-check-" t))
         (notmuch-command (expand-file-name "notmuch" directory))
         (folder (expand-file-name "Inbox" directory))
         (tag-log (expand-file-name "tags" directory))
         (notmuch-custom-imap-post-send-accounts (list (list :local-root directory)))
         (notmuch-custom--sync-process nil)
         (notmuch-custom--sync-queue nil)
         (refreshes 0) process)
    (unwind-protect
        (progn
          (with-temp-file (concat folder ".msf")
            (insert "// <!-- <mdb:mork:z v=\"1.4\"/> -->\n"
                    "< <(a=c)> (80=ns:msg:db:row:scope:msgs:all)(83=message-id)(88=flags)"
                    "(96=ns:msg:db:table:kind:msgs)(9F=ns:msg:db:row:scope:dbfolderinfo:all)"
                    "(A0=ns:msg:db:table:kind:dbfolderinfo)(AC=version)>\n"
                    "{1:^9F {(k^A0:c)} [1(^AC=1)]}\n"
                    "{1:^80 {(k^96:c)} [1(^83=new@example.org)(^88=4)]}\n"))
          (with-temp-file notmuch-command
            (insert "#!/bin/sh\ncase \"$1 $2 $3\" in\n"
                    "'search --format=sexp --output=files') printf '%s\\n' "
                    (shell-quote-argument (prin1-to-string (list (concat folder "/cur/1")))) ";;\n"
                    "'search --output=messages tag:imap-starred') exit 0;;\n"
                    "'tag --batch ') cat >> " (shell-quote-argument tag-log) ";;\n"
                    "*) exit 19;;\nesac\n"))
          (set-file-modes notmuch-command #o700)
          (cl-letf (((symbol-function 'notmuch-refresh-all-buffers) (lambda () (cl-incf refreshes))))
            (setq process (notmuch-custom-sync-thunderbird-starred t))
            (should (process-live-p process))
            (let ((deadline (+ (float-time) 15)))
              (while (and notmuch-custom--sync-process (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should-not notmuch-custom--sync-process)
            (unless (= refreshes 1)
              (ert-fail (with-current-buffer (process-buffer process) (buffer-string))))
            (with-temp-buffer
              (insert-file-contents tag-log)
              (should (string-match-p "+imap-starred" (buffer-string)))
              (should (string-match-p "new@example.org" (buffer-string))))))
      (when (and process (process-live-p process)) (delete-process process))
      (delete-directory directory t))))

(ert-deftest notmuch-msf-queue-preserves-both-manual-options ()
  (let ((notmuch-custom--sync-process nil) (notmuch-custom--sync-queue nil)
        jobs (refreshes 0))
    (cl-letf (((symbol-function 'notmuch-custom--sync-worker-form)
               (lambda (&optional mode)
                 (push mode jobs)
                 '(progn (sleep-for 0.1) (princ "\nNOTMUCH-SYNC-RESULT (:ok t)"))))
              ((symbol-function 'notmuch-refresh-all-buffers) (lambda () (cl-incf refreshes))))
      (unwind-protect
          (progn
            (notmuch-custom-poll-and-refresh)
            (notmuch-custom-sync-thunderbird-starred t)
            (notmuch-custom-sync-thunderbird-starred t)
            (notmuch-custom-sync-imap-starred t)
            (let ((deadline (+ (float-time) 10)))
              (while (and notmuch-custom--sync-process (< (float-time) deadline))
                (accept-process-output nil 0.05)))
            (should-not notmuch-custom--sync-process)
            (should (equal (reverse jobs) '(nil msf t)))
            (should (= refreshes 3)))
        (when (and notmuch-custom--sync-process (process-live-p notmuch-custom--sync-process))
          (delete-process notmuch-custom--sync-process))))))
