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

;;; notmuch-custom-quote-test.el ends here
