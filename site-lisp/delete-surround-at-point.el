;;; https://emacs.stackexchange.com/questions/54659/how-to-delete-surrounding-brackets/54679#54679
(defun my-delete-surround-at-point--find-brackets (pos)
  "Return a pair of buffer positions for the opening & closing
bracket positions. Or nil when nothing is found."
  (save-excursion
    (goto-char pos)
    (when
        (or
         ;; Check if we're on the opening brace.
         (when
             ;; Note that the following check for opening brace
             ;; can be skipped, however it can cause the entire buffer
             ;; to be scanned for an opening brace causing noticeable lag.
             (and
              ;; Opening brace.
              (eq (syntax-class (syntax-after pos)) 4)
              ;; Not escaped.
              (= (logand (skip-syntax-backward "/\\") 1) 0))
           (forward-char 1)
           (if (and (ignore-errors (backward-up-list 1) t) (eq (point) pos))
               t
             ;; Restore location and fall through to the next check.
             (goto-char pos)
             nil))
         ;; Check if we're on the closing or final brace.
         (ignore-errors (backward-up-list 1) t))

      ;; Upon success, return the pair as a list.
      (list (point)
            (progn
              (forward-list)
              (1- (point)))))))

(defun my-delete-surround-at-point ()
  (interactive)
  (let ((range (my-delete-surround-at-point--find-brackets (point))))
    (unless range
      (user-error "No surrounding brackets"))
    (pcase-let ((`(,beg ,end) range))
      ;; For user message.
      (let ((lines (count-lines beg end))
            (beg-char (char-after beg))
            (end-char (char-after end)))

        (save-excursion
          (goto-char end)
          (delete-char 1)
          (goto-char beg)
          (delete-char 1))
        (message
         "Delete surrounding \"%c%c\"%s" beg-char end-char
         (if (> lines 1)
             (format " across %d lines" lines)
           ""))))))
