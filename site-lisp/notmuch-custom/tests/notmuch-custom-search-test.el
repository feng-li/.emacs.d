;;; notmuch-custom-search-test.el --- Search layout tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'notmuch-custom)

(defun notmuch-custom-test--search-subject (subject width)
  (with-temp-buffer
    (insert "12345 ")
    (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) width)))
      (notmuch-custom--search-insert-two-line-subject
       (lambda (&rest _) (ert-fail "Subject unexpectedly delegated"))
       "subject" "%s" (list :subject subject)))
    (buffer-string)))

(ert-deftest notmuch-custom-search-two-line-limit ()
  (should (equal (substring-no-properties
                  (notmuch-custom-test--search-subject "short" 20))
                 "12345 short"))
  (let* ((subject "abcdefghijklmnopqrstuvwxyz0123456789")
         (rendered (notmuch-custom-test--search-subject subject 20))
         (lines (split-string rendered "\n")))
    (should (= (length lines) 2))
    (should (string-prefix-p "      " (cadr lines)))
    (should (string-suffix-p "…" rendered))
    (should (equal (get-text-property 6 'help-echo rendered) subject))
    (dolist (line lines) (should (<= (string-width line) 19)))))

(ert-deftest notmuch-custom-search-chinese-width ()
  (let ((rendered (notmuch-custom-test--search-subject
                   "国际合作部派出办公室批件通知请查收附件并办理相关手续" 20)))
    (should (= (length (split-string rendered "\n")) 2))
    (dolist (line (split-string rendered "\n"))
      (should (<= (string-width line) 19)))
    (should (string-suffix-p "…" rendered))))

(ert-deftest notmuch-custom-search-result-navigation ()
  (with-temp-buffer
    (notmuch-search-mode)
    (let ((inhibit-read-only t)
          (notmuch-search-result-format '(("subject" . "%s"))))
      (advice-add 'notmuch-search-insert-field :around
                  #'notmuch-custom--search-insert-two-line-subject)
      (unwind-protect
          (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 20)))
            (notmuch-search-show-result
             '(:thread "first" :matched 1 :subject "abcdefghijklmnopqrstuvwxyz0123456789abcdef")
             (point-max))
            (notmuch-search-show-result
             '(:thread "second" :matched 1 :subject "Next") (point-max))
            (goto-char (point-min))
            (forward-line 1)
            (should (equal (notmuch-search-find-thread-id t) "first"))
            (notmuch-search-next-thread)
            (should (equal (notmuch-search-find-thread-id t) "second"))
            (notmuch-search-previous-thread)
            (should (= (point) (point-min)))
            (should (equal (notmuch-search-find-subject)
                           "abcdefghijklmnopqrstuvwxyz0123456789abcdef")))
        (advice-remove 'notmuch-search-insert-field
                       #'notmuch-custom--search-insert-two-line-subject)))))

(ert-deftest notmuch-custom-search-unthreaded-display-and-navigation ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (notmuch-tree-mode)
      (setq-local notmuch-tree-unthreaded t)
      (let ((inhibit-read-only t)
            (subject (apply #'concat (make-list 12 "国际合作通知ABC"))))
        (advice-add 'notmuch-tree-format-field :around
                    #'notmuch-custom--tree-mark-subject)
        (advice-add 'notmuch-tree-insert-msg :around
                    #'notmuch-custom--tree-wrap-subject)
        (unwind-protect
            (progn
              (dolist (id '("first" "second"))
                (notmuch-tree-insert-msg
                 (list :id id :match t :date_relative "Today"
                       :headers (list :Subject subject :From "Sender <a@example.org>")
                       :tags '("inbox"))))
              (should (= (count-lines (point-min) (point-max)) 2))
              (goto-char (point-min))
              (let* ((start (text-property-any (point-min) (point-max)
                                               'notmuch-custom-subject t))
                     (display (get-text-property start 'display))
                     (column (save-excursion (goto-char start) (current-column))))
                (should (stringp display))
                (should (= (length (split-string display "\n")) 2))
                (should (string-prefix-p (make-string column ?\s)
                                         (cadr (split-string display "\n"))))
                (should (string-suffix-p "…" display))
                ;; Batch Emacs ignores display strings in vertical-motion.
                (unless noninteractive
                  (should (= (vertical-motion 2) 2))
                  (should (equal (notmuch-tree-get-message-id t) "second"))))
              (goto-char (point-min))
              (notmuch-tree-next-message)
              (should (equal (notmuch-tree-get-message-id t) "second"))
              (notmuch-tree-prev-message)
              (should (equal (notmuch-tree-get-message-id t) "first")))
          (advice-remove 'notmuch-tree-format-field
                         #'notmuch-custom--tree-mark-subject)
          (advice-remove 'notmuch-tree-insert-msg
                         #'notmuch-custom--tree-wrap-subject))))))
