;;; notmuch-custom-quote-test.el --- Reply quote cutoff tests -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'notmuch-custom)

(defun notmuch-custom-quote-test--truncate (text &optional depth)
  "Truncate TEXT at DEPTH, defaulting to three quote levels."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (set-mark (point-max))
    (let ((notmuch-custom-reply-quote-depth (or depth 3)))
      (notmuch-custom-truncate-cited-text))
    (buffer-string)))

(ert-deftest notmuch-custom-quote-keeps-first-depth-block ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     (concat "> Recent message\n>> Earlier message\n"
             ">>> Oldest message\n>>> More oldest text\n"
             ">> Signature\n> Another signature\n"))
    (concat "> Recent message\n>> Earlier message\n"
            ">>> Oldest message\n>>> More oldest text\n"
            ">>> [Older quoted text omitted]\n"))))

(ert-deftest notmuch-custom-quote-discards-later-inline-replies ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     ">>> First block\n>> Later inline reply\n>>> Second block\n")
    ">>> First block\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-preserves-earlier-shallow-text ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     "> A\n>> B\n> C\n>>> D\n>> Signature\n")
    "> A\n>> B\n> C\n>>> D\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-no-cutoff-without-limit-depth ()
  (let ((text "> Recent\n>> Earlier\n> Inline reply\n>> Signature\n"))
    (should (equal (notmuch-custom-quote-test--truncate text) text))))

(ert-deftest notmuch-custom-quote-discards-deeper-text-and-tail ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     "> Recent\n>>>> Too old\n>> Signature\n")
    "> Recent\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-deeper-line-ends-first-block ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     ">>> Retained\n>>>> Too old\n>>> Later text\n> Signature\n")
    ">>> Retained\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-handles-spaced-prefixes ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     "> Recent\n> > > First\n  >\t> > Second\n> > Signature\n")
    "> Recent\n>>> First\n>>> Second\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-keeps-quoted-blank-lines-in-block ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     ">>> First\n>>>\n>>> Second\n>> Signature\n")
    ">>> First\n>>> \n>>> Second\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-keeps-only-one-omission-marker ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     (concat ">>> Retained\n>>> [Older quoted text omitted]\n"
             "> Signature\n>>> [Older quoted text omitted]\n"))
    ">>> Retained\n>>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-no-marker-without-omitted-text ()
  (dolist (text '(">>> First\n>>> Second\n" ">>> First\n>>> Second"))
    (should (equal (notmuch-custom-quote-test--truncate text) text))))

(ert-deftest notmuch-custom-quote-no-marker-for-unquoted-whitespace-tail ()
  (should
   (equal (notmuch-custom-quote-test--truncate ">>> Retained\n\n \t\n")
          ">>> Retained\n")))

(ert-deftest notmuch-custom-quote-respects-configured-depth ()
  (should
   (equal
    (notmuch-custom-quote-test--truncate
     "> Recent\n>> Earlier\n>> More earlier\n> Signature\n" 2)
    "> Recent\n>> Earlier\n>> More earlier\n>> [Older quoted text omitted]\n")))

(ert-deftest notmuch-custom-quote-unlimited-does-not-alter-text ()
  (with-temp-buffer
    (insert ">>> Retained\n>>>> Deeper\n> Later inline reply\n")
    (let ((original (buffer-string))
          (notmuch-custom-reply-quote-depth nil))
      (goto-char (point-min))
      (set-mark (point-max))
      (notmuch-custom-truncate-cited-text)
      (should (equal (buffer-string) original)))))

(ert-deftest notmuch-custom-quote-only-changes-cited-region ()
  (with-temp-buffer
    (insert "My new reply\n")
    (let ((start (point))
          (notmuch-custom-reply-quote-depth 3))
      (insert "> Recent\n>>> Retained\n>> Old signature\n")
      (let ((end (point)))
        (insert "\n-- \nMy own signature\n")
        ;; Also exercise point after mark rather than before it.
        (goto-char end)
        (set-mark start)
        (notmuch-custom-truncate-cited-text))
      (should
       (equal (buffer-string)
              (concat "My new reply\n> Recent\n>>> Retained\n"
                      ">>> [Older quoted text omitted]\n"
                      "\n-- \nMy own signature\n"))))))

(ert-deftest notmuch-custom-quote-cutoff-is-idempotent ()
  (let ((once (notmuch-custom-quote-test--truncate
               "> Recent\n>>> Retained\n>> Old signature\n")))
    (should (equal (notmuch-custom-quote-test--truncate once) once))))

(ert-deftest notmuch-custom-quote-counts-new-reply-prefix ()
  (with-temp-buffer
    (insert "Recent\n> Earlier\n>> Oldest\n>> More oldest\n> Old signature\n")
    (goto-char (point-min))
    (set-mark (point-max))
    (let ((message-yank-prefix "> ")
          (message-fill-column 72)
          (notmuch-custom-reply-quote-depth 3))
      (notmuch-custom-message-fill-setup)
      (dolist (function message-indent-citation-function)
        (funcall function)))
    (should
     (equal (buffer-string)
            (concat "> Recent\n>> Earlier\n>>> Oldest More oldest\n"
                    ">>> [Older quoted text omitted]\n")))))

(defun notmuch-custom-quote-test--collapse (text)
  "Collapse repeated blank lines in quoted TEXT."
  (with-temp-buffer
    (insert text)
    (notmuch-custom-collapse-cited-blank-lines (point-min) (point-max))
    (buffer-string)))

(ert-deftest notmuch-custom-quote-collapses-repeated-blank-lines ()
  (should
   (equal
    (notmuch-custom-quote-test--collapse
     "> First paragraph\n> \n>\n> \n> Second paragraph\n>\n> Third paragraph\n")
    "> First paragraph\n> \n> Second paragraph\n>\n> Third paragraph\n")))

(ert-deftest notmuch-custom-quote-collapses-mixed-blank-prefixes ()
  (should
   (equal
    (notmuch-custom-quote-test--collapse
     "> A\r\n> > \r\n>>\n \t\n\u00a0>\u00a0>\u00a0\n\n> B\n")
    "> A\r\n> > \r\n> B\n")))

(ert-deftest notmuch-custom-quote-blank-cleanup-is-idempotent ()
  (let* ((text "> A\n>\n>\n> B\n> \n> C\n")
         (once (notmuch-custom-quote-test--collapse text)))
    (should (equal (notmuch-custom-quote-test--collapse once) once))))

(ert-deftest notmuch-custom-quote-collapses-blank-final-line ()
  (should (equal (notmuch-custom-quote-test--collapse "> A\n>\n> ")
                 "> A\n>\n")))

(ert-deftest notmuch-custom-quote-blank-cleanup-only-changes-region ()
  (with-temp-buffer
    (insert "My reply\n\n\n")
    (let ((start (point)))
      (insert "> A\n>\n>\n> B\n")
      (let ((end (point)))
        (insert "\n\n-- \nMy signature\n\n\n")
        (goto-char end)
        (set-mark start)
        (notmuch-custom-collapse-cited-blank-lines)))
    (should
     (equal (buffer-string)
            "My reply\n\n\n> A\n>\n> B\n\n\n-- \nMy signature\n\n\n"))))

(ert-deftest notmuch-custom-quote-blank-cleanup-preserves-partial-lines ()
  (with-temp-buffer
    (insert ">\n>\n>\n>\n> \n> Outside\n")
    (notmuch-custom-collapse-cited-blank-lines 2 10)
    (should (equal (buffer-string) ">\n>\n> \n> Outside\n"))))

(ert-deftest notmuch-custom-quote-blank-cleanup-without-mark-is-noop ()
  (with-temp-buffer
    (insert ">\n>\n")
    (notmuch-custom-collapse-cited-blank-lines)
    (should (equal (buffer-string) ">\n>\n"))))

(ert-deftest notmuch-custom-quote-pipeline-cleans-blank-lines-by-default ()
  (with-temp-buffer
    (insert "From: Sender <sender@example.test>\n\nRecent\n\n\n\nAnother paragraph\n> Earlier\n>> Oldest\n> Old signature\n")
    (goto-char (point-min))
    (set-mark (point-max))
    (let ((message-yank-prefix "> ")
          (message-fill-column 72)
          (notmuch-custom-reply-quote-depth 3))
      (notmuch-custom-message-fill-setup)
      (should (memq #'notmuch-custom-collapse-cited-blank-lines
                    message-indent-citation-function))
      (dolist (function message-indent-citation-function)
        (funcall function)))
    (should
     (equal (buffer-string)
            (concat "> Recent\n> \n> Another paragraph\n>> Earlier\n"
                    ">>> Oldest\n>>> [Older quoted text omitted]\n")))))

;;; notmuch-custom-quote-test.el ends here
