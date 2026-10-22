;;; notmuch-custom-charset-test.el --- Charset fallback tests -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'notmuch-custom)

(defun notmuch-custom-test--decode (raw text &optional charset)
  "Exercise the fallback on RAW and TEXT with optional CHARSET."
  (cl-letf (((symbol-function 'notmuch-get-bodypart-binary)
             (lambda (&rest _) raw)))
    (notmuch-custom--decode-unlabeled-chinese-text
     (lambda (&rest _) text)
     '(:headers (:From "Someone <someone@example.org>"))
     (list :content-type "text/plain" :content-charset charset) nil)))

(ert-deftest notmuch-custom-charset-unlabeled-chinese ()
  (dolist (coding '(gb2312 gbk gb18030-unix))
    (let* ((expected "您好，任务已经完成。\r\n请查看附件。")
           (raw (encode-coding-string expected coding)))
      (should (equal (notmuch-custom-test--decode raw "�") expected)))))

(ert-deftest notmuch-custom-charset-preserve-utf8 ()
  ;; Even an intentional replacement character must not trigger GB decoding.
  (let ((text "中文 �"))
    (should (equal (notmuch-custom-test--decode
                    (encode-coding-string text 'utf-8-unix) text) text))))

(ert-deftest notmuch-custom-charset-preserve-labeled-and-valid ()
  (cl-letf (((symbol-function 'notmuch-get-bodypart-binary)
             (lambda (&rest _) (ert-fail "Unexpected raw fetch"))))
    (dolist (case '(("�" "utf-8") ("中文" nil) ("ASCII" nil)))
      (should (equal
               (notmuch-custom--decode-unlabeled-chinese-text
                (lambda (&rest _) (car case)) nil
                (list :content-type "text/plain" :content-charset (cadr case)) nil)
               (car case))))))

(ert-deftest notmuch-custom-charset-reject-invalid-or-non-chinese ()
  (dolist (raw (list (unibyte-string #xff)
                    (encode-coding-string "é" 'gb18030-unix)))
    (should (equal (notmuch-custom-test--decode raw "�") "�"))))
