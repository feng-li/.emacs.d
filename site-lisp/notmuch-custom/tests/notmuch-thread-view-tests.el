;;; notmuch-thread-view-tests.el --- Thread tree integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'notmuch-custom-thread-view)

(defun notmuch-thread-view-test--message (id &optional match subject)
  "Make a renderable tree message with ID, MATCH state, and SUBJECT."
  (list :id id :match match :tags '("inbox") :date_relative "today"
        :headers (list :Subject (or subject "A shared subject")
                       :From "Sender <s@example.test>")))

(defun notmuch-thread-view-test--thread ()
  "A parent, two reply branches, and one grandchild in the older branch."
  (list (list (notmuch-thread-view-test--message "parent" t)
              (list (list (notmuch-thread-view-test--message "older" nil)
                          (list (list (notmuch-thread-view-test--message "child" t) nil)))
                    (list (notmuch-thread-view-test--message "newer" t) nil)))))

(defun notmuch-thread-view-test--rows ()
  "Read all real message rows in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (rows)
      (while (not (eobp))
        (when (notmuch-tree-get-message-id)
          (push (copy-sequence (notmuch-tree-get-message-properties)) rows))
        (forward-line))
      (nreverse rows))))

(ert-deftest notmuch-thread-view-opens-complete-folder-thread-at-selected-message ()
  (with-temp-buffer
    (setq major-mode 'notmuch-tree-mode)
    (setq-local notmuch-tree-unthreaded t)
    (setq-local notmuch-tree-query-context "tag:inbox")
    (setq-local notmuch-search-oldest-first nil)
    (let (arguments)
      (cl-letf (((symbol-function 'notmuch-tree-get-message-id)
                 (lambda (&optional _) "id:chosen@example.test"))
                ((symbol-function 'notmuch-tree)
                 (lambda (&rest args) (setq arguments args))))
        (notmuch-custom-open-thread-tree)
        (should (equal arguments
                       (list "id:chosen@example.test" nil "id:chosen@example.test"
                             nil t nil (current-buffer) nil nil)))))))

(ert-deftest notmuch-thread-view-search-opens-only-selected-thread ()
  (with-temp-buffer
    (setq major-mode 'notmuch-search-mode)
    (setq-local notmuch-search-query-string "tag:unread")
    (let (arguments)
      (cl-letf (((symbol-function 'notmuch-search-find-thread-id)
                 (lambda () "thread:123"))
                ((symbol-function 'notmuch-tree)
                 (lambda (&rest args) (setq arguments args))))
        (notmuch-custom-open-thread-tree)
        (should (equal (car arguments) "thread:123"))
        (should-not (nth 1 arguments))
        (should-not (nth 2 arguments))
        (should (nth 4 arguments))
        (should-not (nth 5 arguments))
        (should (eq (nth 6 arguments) (current-buffer)))
        (should-not (nth 8 arguments))))))

(ert-deftest notmuch-thread-view-ret-inside-tree-keeps-normal-prefix-behavior ()
  (with-temp-buffer
    (setq major-mode 'notmuch-tree-mode)
    (setq-local notmuch-tree-unthreaded nil)
    (let (prefix)
      (cl-letf (((symbol-function 'notmuch-tree-show-message)
                 (lambda (arg) (setq prefix arg)))
                ((symbol-function 'notmuch-tree)
                 (lambda (&rest _) (ert-fail "Must not open another tree"))))
        (notmuch-custom-open-thread-tree '(4))
        (should (equal prefix '(4)))))))

(ert-deftest notmuch-thread-view-empty-row-and-wrong-mode-rejected ()
  (with-temp-buffer
    (should-error (notmuch-custom-open-thread-tree) :type 'user-error)
    (setq major-mode 'notmuch-tree-mode)
    (setq-local notmuch-tree-unthreaded t)
    (should-error (notmuch-custom-open-thread-tree) :type 'user-error)))

(ert-deftest notmuch-thread-view-reverses-real-renderer-with-indentation ()
  (with-temp-buffer
    (notmuch-tree-mode)
    (let* ((thread (notmuch-thread-view-test--thread))
           (before (copy-tree thread))
           (notmuch-custom-tree-reverse-order t)
           (inhibit-read-only t))
      (setq-local notmuch-tree-unthreaded nil)
      (notmuch-custom--tree-insert-reversed #'notmuch-tree-insert-forest-thread thread)
      (let ((rows (notmuch-thread-view-test--rows)))
        (should (equal (mapcar (lambda (msg) (plist-get msg :id)) rows)
                       '("newer" "child" "older" "parent")))
        (should (equal (mapcar (lambda (msg) (length (plist-get msg :tree-status))) rows)
                       '(3 4 3 2)))
        (should (equal (mapcar (lambda (msg) (plist-get msg :first)) rows)
                       '(t nil nil nil)))
        (should (equal (mapcar (lambda (msg) (plist-get msg :level)) rows)
                       '(1 2 2 2)))
        (should-not (plist-get (nth 2 rows) :match))
        (should (equal (plist-get (car rows) :tags) '("inbox"))))
      (should (equal thread before))
      (should (string-match-p "╭─►" (buffer-string)))
      (should (string-match-p "┴►" (buffer-string)))
      ;; The bottom parent anchors the subject for the replies above it.
      (goto-char (point-min))
      (should-not (search-forward "A shared subject" (line-end-position) t))
      (goto-char (point-max))
      (forward-line -1)
      (should (search-forward "A shared subject" (line-end-position) t)))))

(ert-deftest notmuch-thread-view-subject-groups-are-anchored-at-the-bottom ()
  "Keep the original topic on the root and a changed topic on its first reply."
  (with-temp-buffer
    (notmuch-tree-mode)
    (let* ((notmuch-tree-result-format '(("subject" . "%s")))
           (notmuch-custom-tree-reverse-order t)
           (inhibit-read-only t)
           (root (notmuch-thread-view-test--message
                  "root" t "You have a new Editor Assignment"))
           (reply (notmuch-thread-view-test--message
                   "reply" t "Re: You have a new Editor Assignment"))
           (changed (notmuch-thread-view-test--message "changed" t "Hello"))
           (latest (notmuch-thread-view-test--message "latest" t "Re: Hello"))
           (thread (list (list root
                               (list (list reply
                                           (list (list changed
                                                       (list (list latest nil))))))))))
      (notmuch-custom--tree-insert-reversed #'notmuch-tree-insert-forest-thread thread)
      (should (equal (split-string (buffer-string) "\n" t)
                     '(" ..." "Hello" " ..." "You have a new Editor Assignment")))
      ;; Tag changes redraw individual rows using their saved comparison.
      (let ((before (buffer-string)))
        (goto-char (point-min))
        (while (not (eobp))
          (notmuch-tree-set-tags '("inbox" "flagged"))
          (notmuch-tree-refresh-result)
          (forward-line))
        (should (equal (buffer-string) before))))))

(ert-deftest notmuch-thread-view-subject-comparison-is-sanitized-and-thread-local ()
  (with-temp-buffer
    (notmuch-tree-mode)
    (let* ((notmuch-tree-result-format '(("subject" . "%s")))
           (notmuch-custom-tree-reverse-order t)
           (inhibit-read-only t)
           (root (notmuch-thread-view-test--message "root" t "A\ttopic"))
           (reply (notmuch-thread-view-test--message "reply" t "Re: A\ttopic")))
      (dotimes (_ 2)
        (notmuch-custom--tree-insert-reversed
         #'notmuch-tree-insert-forest-thread (list (list root (list (list reply nil))))))
      (should (equal (split-string (buffer-string) "\n" t)
                     '(" ..." "A topic" " ..." "A topic"))))))

(ert-deftest notmuch-thread-view-preserves-target-and-auto-preview ()
  (with-temp-buffer
    (notmuch-tree-mode)
    (setq-local notmuch-tree-target-msg "id:older")
    (setq-local notmuch-tree-open-target t)
    (let ((inhibit-read-only t)
          (notmuch-custom-tree-reverse-order t)
          previews)
      (cl-letf (((symbol-function 'notmuch-tree-show-message-in)
                 (lambda () (push (notmuch-tree-get-message-id t) previews)))
                ((symbol-function 'notmuch-tree-command-hook) #'ignore))
        (notmuch-custom--tree-insert-reversed
         #'notmuch-tree-insert-forest-thread (notmuch-thread-view-test--thread))
        (should (equal previews '("older")))
        (should (equal (notmuch-tree-get-message-id t) "older"))))))

(ert-deftest notmuch-thread-view-search-target-defaults-to-first-reverse-match ()
  (with-temp-buffer
    (notmuch-tree-mode)
    (let ((inhibit-read-only t)
          (notmuch-custom-tree-reverse-order t))
      (notmuch-custom--tree-insert-reversed
       #'notmuch-tree-insert-forest-thread (notmuch-thread-view-test--thread))
      (should (equal (notmuch-tree-get-message-id t) "newer")))))

(ert-deftest notmuch-thread-view-native-rendering-for-flat-lists-or-disabled-option ()
  (dolist (settings '((t t) (nil nil)))
    (let ((notmuch-tree-unthreaded (car settings))
          (notmuch-custom-tree-reverse-order (cadr settings))
          (thread (notmuch-thread-view-test--thread))
          received)
      (notmuch-custom--tree-insert-reversed
       (lambda (arg) (setq received arg)) thread)
      (should (eq received thread)))))

(ert-deftest notmuch-thread-view-mirrors-only-box-drawing-characters ()
  (should (equal (notmuch-custom--mirror-tree-symbol "╰┬─│├►+-") "╭┴─│├►+-")))

(ert-deftest notmuch-thread-view-up-down-preview-all-messages-and-clamp-boundaries ()
  (with-temp-buffer
    (notmuch-tree-mode)
    (let ((inhibit-read-only t)
          (notmuch-custom-tree-reverse-order t)
          previews)
      (notmuch-custom--tree-insert-reversed
       #'notmuch-tree-insert-forest-thread (notmuch-thread-view-test--thread))
      (goto-char (point-max))
      (insert "End of search results.\n")
      (goto-char (point-min))
      (setq-local notmuch-tree-message-window (selected-window))
      (cl-letf (((symbol-function 'notmuch-tree-show-message-in)
                 (lambda () (push (notmuch-tree-get-message-id t) previews))))
        (notmuch-custom-tree-previous-message)
        (should-not previews)
        (notmuch-custom-tree-next-message)
        (notmuch-custom-tree-next-message)
        (notmuch-custom-tree-next-message 20)
        (should (equal (notmuch-tree-get-message-id t) "parent"))
        (notmuch-custom-tree-next-message)
        (should (equal (nreverse previews) '("child" "older" "parent")))
        (notmuch-custom-tree-previous-message 20)
        (should (equal (notmuch-tree-get-message-id t) "newer"))))))

(ert-deftest notmuch-thread-view-closed-preview-reopens-even-at-boundary ()
  (with-temp-buffer
    (notmuch-tree-mode)
    (let ((inhibit-read-only t)
          (notmuch-custom-tree-reverse-order t)
          opened)
      (notmuch-custom--tree-insert-reversed
       #'notmuch-tree-insert-forest-thread (notmuch-thread-view-test--thread))
      (goto-char (point-min))
      (setq-local notmuch-tree-message-window nil)
      (cl-letf (((symbol-function 'notmuch-tree-show-message-in)
                 (lambda () (setq opened (notmuch-tree-get-message-id t)))))
        (notmuch-custom-tree-previous-message)
        (should (equal opened "newer"))))))

(ert-deftest notmuch-thread-view-flat-folder-arrows-do-not-preview ()
  (with-temp-buffer
    (insert "one\ntwo\n")
    (goto-char (point-min))
    (setq-local notmuch-tree-unthreaded t)
    (cl-letf (((symbol-function 'notmuch-tree-show-message-in)
               (lambda () (ert-fail "Flat folders must not auto-preview"))))
      (notmuch-custom-tree-next-message)
      (should (= (line-number-at-pos) 2))
      (notmuch-custom-tree-previous-message)
      (should (= (line-number-at-pos) 1)))))

(ert-deftest notmuch-thread-view-native-split-preview-follows-arrow-navigation ()
  "Exercise the real window/preview path without reading or tagging real mail."
  (let ((tree (generate-new-buffer " *reverse-tree-window-test*"))
        (notmuch-show-mark-read-tags nil)
        previews)
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (switch-to-buffer tree)
          (notmuch-tree-mode)
          (let ((inhibit-read-only t)
                (notmuch-custom-tree-reverse-order t))
            (notmuch-custom--tree-insert-reversed
             #'notmuch-tree-insert-forest-thread (notmuch-thread-view-test--thread)))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'notmuch-show)
                     (lambda (query &rest _)
                       (let ((preview (generate-new-buffer " *reverse-tree-preview-test*")))
                         (push (cons query preview) previews)
                         (switch-to-buffer preview)
                         preview))))
            (notmuch-custom-tree-next-message)
            (should (eq (current-buffer) tree))
            (should (window-live-p notmuch-tree-message-window))
            (should (equal (caar previews) "id:child"))
            (should (eq (window-buffer notmuch-tree-message-window) (cdar previews)))
            (notmuch-custom-tree-next-message)
            (should (equal (caar previews) "id:older"))
            (should (eq (window-buffer notmuch-tree-message-window) (cdar previews)))
            (notmuch-tree-close-message-window)))
      (when (buffer-live-p tree) (kill-buffer tree))
      (dolist (entry previews)
        (when (buffer-live-p (cdr entry)) (kill-buffer (cdr entry)))))))

(ert-deftest notmuch-thread-view-setup-keys-and-advice-are-idempotent ()
  (let ((notmuch-tree-mode-map (copy-keymap notmuch-tree-mode-map))
        (notmuch-search-mode-map (copy-keymap notmuch-search-mode-map))
        (notmuch-tree-show-out t)
        (unthreaded-setting notmuch-unthreaded-show-out)
        (show-ret (lookup-key notmuch-show-mode-map (kbd "RET"))))
    (unwind-protect
        (progn
          (notmuch-custom-thread-view-setup)
          (notmuch-custom-thread-view-setup)
          (should-not notmuch-tree-show-out)
          (should (eq notmuch-unthreaded-show-out unthreaded-setting))
          (should (eq (lookup-key notmuch-tree-mode-map (kbd "RET"))
                      #'notmuch-custom-open-thread-tree))
          (should (eq (lookup-key notmuch-search-mode-map [mouse-1])
                      #'notmuch-custom-open-thread-tree))
          (should (eq (lookup-key notmuch-tree-mode-map [down])
                      #'notmuch-custom-tree-next-message))
          (should (eq (lookup-key notmuch-tree-mode-map [up])
                      #'notmuch-custom-tree-previous-message))
          (should (eq (lookup-key notmuch-tree-mode-map (kbd "C-c C-e"))
                      #'notmuch-custom-extract-thread-attachments))
          (should (eq (lookup-key notmuch-show-mode-map (kbd "RET")) show-ret)))
      (advice-remove 'notmuch-tree-insert-forest-thread
                     #'notmuch-custom--tree-insert-reversed))))

(provide 'notmuch-thread-view-tests)
;;; notmuch-thread-view-tests.el ends here
