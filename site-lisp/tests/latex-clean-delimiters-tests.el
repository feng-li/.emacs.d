;;; latex-clean-delimiters-tests.el --- Save cleanup regressions -*- lexical-binding: t; -*-

(require 'ert)

;; Load just the definition under test, without initializing the desktop or
;; installing packages through init.el.
(let ((init (expand-file-name "../../init.el"
                              (file-name-directory (or load-file-name buffer-file-name)))))
  (with-temp-buffer
    (insert-file-contents init)
    (goto-char (point-min))
    (search-forward "(defun my-latex-clean-delimiters ()")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(defun latex-clean-delimiters-test--clean (text)
  (with-temp-buffer
    (insert text)
    (my-latex-clean-delimiters)
    (buffer-string)))

(ert-deftest latex-clean-delimiters-converts-complete-pairs ()
  (should (equal (latex-clean-delimiters-test--clean
                  "Before \\(x + y\\), then \\(a\nb\\).")
                 "Before $x + y$, then $a\nb$.")))

(ert-deftest latex-clean-delimiters-preserves-unmatched-tokens ()
  (dolist (text '("Unclosed \\(x" "Unopened x\\)" "\\) before \\("))
    (should (equal (latex-clean-delimiters-test--clean text) text)))
  (should (equal (latex-clean-delimiters-test--clean
                  "\\) then \\(x\\) then \\(orphan")
                 "\\) then $x$ then \\(orphan")))

(ert-deftest latex-clean-delimiters-preserves-shortstack-line-breaks ()
  (let ((row (concat "\\shortstack{Setup/checks\\\\(s)} & "
                     "\\shortstack{MH\\\\(s)} & "
                     "\\shortstack{Total\\\\(s)}")))
    (should (equal (latex-clean-delimiters-test--clean row) row))
    (should (equal (latex-clean-delimiters-test--clean
                    (concat row " and \\(s\\)"))
                   (concat row " and $s$")))))

(ert-deftest latex-clean-delimiters-checks-backslash-parity-at-both-ends ()
  (let ((line-break (make-string 2 ?\\)))
    (should (equal (latex-clean-delimiters-test--clean
                    (concat line-break "(s) and \\(x" line-break ") + y\\)"))
                   (concat line-break "(s) and $x" line-break ") + y$")))
    ;; Three backslashes mean a line break followed by a real delimiter.
    (should (equal (latex-clean-delimiters-test--clean
                    (concat line-break "\\(x" line-break "\\)"))
                   (concat line-break "$x" line-break "$")))))

(ert-deftest latex-clean-delimiters-keeps-bold-cleanup-and-is-idempotent ()
  (let* ((text "**Text** \\(x\\) and \\\\(s)")
         (cleaned (latex-clean-delimiters-test--clean text)))
    (should (equal cleaned "Text $x$ and \\\\(s)"))
    (should (equal (latex-clean-delimiters-test--clean cleaned) cleaned))))

;;; latex-clean-delimiters-tests.el ends here
