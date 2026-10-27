;;; tty-emoji-fallback-tests.el --- Safe terminal emoji tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'tty-emoji-fallback)

(defmacro tty-emoji-test--with-buffer (text &rest body)
  "Display TEXT in an isolated window and run BODY with fallback enabled."
  (declare (indent 1) (debug t))
  `(let ((tty-emoji-fallback--windows (make-hash-table :test #'eq))
         (tty-emoji-fallback-mode t)
         (tty-emoji-fallback-placeholder "[emoji]"))
     (unwind-protect
         (save-window-excursion
           (with-temp-buffer
             (insert ,text)
             (set-window-buffer (selected-window) (current-buffer))
             (goto-char (point-min))
             (set-window-start (selected-window) (point-min))
             ,@body))
       (let (windows)
         (maphash (lambda (window _state) (push window windows))
                  tty-emoji-fallback--windows)
         (mapc #'tty-emoji-fallback--clear-window windows)))))

(defun tty-emoji-test--overlays (&optional window)
  "Return WINDOW's fallback overlays in buffer order."
  (sort (copy-sequence
         (cdr (gethash (or window (selected-window)) tty-emoji-fallback--windows)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun tty-emoji-test--covered-text ()
  "Return strings hidden by fallback overlays in the selected window."
  (mapcar (lambda (overlay)
            (buffer-substring-no-properties (overlay-start overlay) (overlay-end overlay)))
          (tty-emoji-test--overlays)))

(ert-deftest tty-emoji-fallback-replaces-narrow-and-variation-sequences ()
  (tty-emoji-test--with-buffer "Ⓜ️ Ⓜ ™️ ©️ ↔️ end"
    (tty-emoji-fallback--redisplay (selected-window))
    (should (equal (tty-emoji-test--covered-text) '("Ⓜ️" "Ⓜ" "™️" "©️" "↔️")))
    (dolist (overlay (tty-emoji-test--overlays))
      (should (equal (overlay-get overlay 'display) "[emoji]"))
      (should (eq (overlay-get overlay 'window) (selected-window))))))

(ert-deftest tty-emoji-fallback-preserves-ordinary-wide-emoji-and-nonemoji ()
  (tty-emoji-test--with-buffer "Hello 中文 αβ → ≈ ∑ ─│ +-#*123 😀 🚀 ✅"
    (tty-emoji-fallback--redisplay (selected-window))
    (should-not (tty-emoji-test--overlays))))

(ert-deftest tty-emoji-fallback-matches-whole-complex-sequences ()
  (tty-emoji-test--with-buffer "👩‍💻 👨‍👩‍👧‍👦 👍🏽 🇨🇳 1️⃣ #️⃣ 🏳️‍🌈 🏴\U000e0067\U000e0062\U000e0065\U000e006e\U000e0067\U000e007f"
    (tty-emoji-fallback--redisplay (selected-window))
    (should (equal (tty-emoji-test--covered-text)
                   '("👩‍💻" "👨‍👩‍👧‍👦" "👍🏽" "🇨🇳" "1️⃣" "#️⃣" "🏳️‍🌈"
                     "🏴\U000e0067\U000e0062\U000e0065\U000e006e\U000e0067\U000e007f")))))

(ert-deftest tty-emoji-fallback-supports-unqualified-and-text-variation-forms ()
  (tty-emoji-test--with-buffer "Ⓜ︎ 1⃣ 🏳‍🌈"
    (tty-emoji-fallback--redisplay (selected-window))
    (should (equal (tty-emoji-test--covered-text) '("Ⓜ︎" "1⃣" "🏳‍🌈")))))

(ert-deftest tty-emoji-fallback-does-not-change-text-properties-or-modification-state ()
  (tty-emoji-test--with-buffer "x Ⓜ️ y"
    (add-text-properties (point-min) (point-max) '(face bold))
    (set-buffer-modified-p nil)
    (let ((before (buffer-string))
          (tick (buffer-chars-modified-tick))
          (undo buffer-undo-list)
          (width-table char-width-table)
          (display-table standard-display-table))
      (tty-emoji-fallback--redisplay (selected-window))
      (should (equal-including-properties (buffer-string) before))
      (should (= tick (buffer-chars-modified-tick)))
      (should (eq undo buffer-undo-list))
      (should-not (buffer-modified-p))
      (should (eq char-width-table width-table))
      (should (eq standard-display-table display-table)))))

(ert-deftest tty-emoji-fallback-works-in-read-only-buffer ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (setq buffer-read-only t)
    (tty-emoji-fallback--redisplay (selected-window))
    (should (= (length (tty-emoji-test--overlays)) 1))))

(ert-deftest tty-emoji-fallback-saves-original-file-bytes ()
  (let ((file (make-temp-file "tty-emoji-original-")))
    (unwind-protect
        (tty-emoji-test--with-buffer "title: Ⓜ️ 中文 👩‍💻\n"
          (let ((expected (encode-coding-string (buffer-string) 'utf-8-unix)))
            (tty-emoji-fallback--redisplay (selected-window))
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (point-min) (point-max) file nil 'silent))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally file)
              (should (equal (buffer-string) expected)))))
      (delete-file file))))

(ert-deftest tty-emoji-fallback-restricts-scan-to-visible-logical-lines ()
  (tty-emoji-test--with-buffer "Ⓜ️ first\nⓂ️ second\nⓂ️ third\n"
    (let ((window (selected-window)))
      (cl-letf (((symbol-function 'window-end)
                 (lambda (&rest _) 4)))
        (tty-emoji-fallback--redisplay window))
      (should (= (length (tty-emoji-test--overlays)) 1))
      (should (= (overlay-start (car (tty-emoji-test--overlays))) 1))
      (goto-char (point-min))
      (forward-line 2)
      (set-window-start window (point))
      (cl-letf (((symbol-function 'window-end)
                 (lambda (&rest _) (point-max))))
        (tty-emoji-fallback--redisplay window))
      (should (= (length (tty-emoji-test--overlays)) 1))
      (should (= (line-number-at-pos (overlay-start (car (tty-emoji-test--overlays)))) 3)))))

(ert-deftest tty-emoji-fallback-respects-narrowing ()
  (tty-emoji-test--with-buffer "Ⓜ️ first\nⓂ️ second\n"
    (forward-line)
    (narrow-to-region (point) (point-max))
    (set-window-start (selected-window) (point-min))
    (tty-emoji-fallback--redisplay (selected-window))
    (should (= (length (tty-emoji-test--overlays)) 1))
    (should (= (overlay-start (car (tty-emoji-test--overlays))) (point-min)))))

(ert-deftest tty-emoji-fallback-caches-unchanged-window ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (tty-emoji-fallback--redisplay (selected-window))
    (let ((first (car (tty-emoji-test--overlays))))
      (tty-emoji-fallback--redisplay (selected-window))
      (should (eq first (car (tty-emoji-test--overlays)))))))

(ert-deftest tty-emoji-fallback-updates-after-edit-and-major-mode-change ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (tty-emoji-fallback--redisplay (selected-window))
    (goto-char (point-max))
    (insert " 👩‍💻")
    (fundamental-mode)
    (tty-emoji-fallback--redisplay (selected-window))
    (should (equal (tty-emoji-test--covered-text) '("Ⓜ️" "👩‍💻")))
    (erase-buffer)
    (insert "normal")
    (tty-emoji-fallback--redisplay (selected-window))
    (should-not (tty-emoji-test--overlays))))

(ert-deftest tty-emoji-fallback-gui-window-does-not-replace-text ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
      (tty-emoji-fallback--redisplay (selected-window)))
    (should-not (tty-emoji-test--overlays))))

(ert-deftest tty-emoji-fallback-shared-buffer-is-window-specific ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (let* ((tty (selected-window))
           (gui (split-window-right)))
      (set-window-buffer gui (current-buffer))
      (cl-letf (((symbol-function 'display-graphic-p)
                 (lambda (&rest _) (eq (selected-window) gui))))
        (tty-emoji-fallback--redisplay tty)
        (with-selected-window gui (tty-emoji-fallback--redisplay gui)))
      (should (= (length (tty-emoji-test--overlays tty)) 1))
      (should-not (tty-emoji-test--overlays gui))
      (should (equal (get-char-property 1 'display tty) "[emoji]"))
      (should-not (get-char-property 1 'display gui)))))

(ert-deftest tty-emoji-fallback-two-terminal-windows-own-separate-overlays ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (let ((one (selected-window))
          (two (split-window-right)))
      (set-window-buffer two (current-buffer))
      (tty-emoji-fallback--redisplay one)
      (tty-emoji-fallback--redisplay two)
      (should (= (length (tty-emoji-test--overlays one)) 1))
      (should (= (length (tty-emoji-test--overlays two)) 1))
      (should-not (eq (car (tty-emoji-test--overlays one))
                      (car (tty-emoji-test--overlays two)))))))

(ert-deftest tty-emoji-fallback-cleans-up-deleted-windows-and-switched-buffers ()
  (tty-emoji-test--with-buffer "Ⓜ️"
    (let ((other (split-window-right)))
      (set-window-buffer other (current-buffer))
      (tty-emoji-fallback--redisplay other)
      (let ((overlay (car (tty-emoji-test--overlays other))))
        (delete-window other)
        (tty-emoji-fallback--prune)
        (should-not (overlay-buffer overlay))
        (should-not (gethash other tty-emoji-fallback--windows))))
    (tty-emoji-fallback--redisplay (selected-window))
    (let ((overlay (car (tty-emoji-test--overlays))))
      (set-window-buffer (selected-window) (get-buffer-create " *emoji-test-other*"))
      (tty-emoji-fallback--prune)
      (should-not (overlay-buffer overlay))
      (kill-buffer " *emoji-test-other*"))))

(ert-deftest tty-emoji-fallback-respects-existing-display-and-invisible-text ()
  (tty-emoji-test--with-buffer "Ⓜ️ Ⓜ️"
    (put-text-property 1 3 'display "existing")
    (put-text-property 4 6 'invisible t)
    (tty-emoji-fallback--redisplay (selected-window))
    (should-not (tty-emoji-test--overlays))
    (should (equal (get-text-property 1 'display) "existing"))))

(ert-deftest tty-emoji-fallback-toggle-clears-overlays-and-hooks ()
  (let ((tty-emoji-fallback--windows (make-hash-table :test #'eq))
        (tty-emoji-fallback-mode nil)
        (pre-redisplay-functions nil))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (insert "Ⓜ️")
            (set-window-buffer (selected-window) (current-buffer))
            (tty-emoji-fallback-mode 1)
            (should (memq #'tty-emoji-fallback--redisplay pre-redisplay-functions))
            (let ((overlay (car (tty-emoji-test--overlays))))
              (should overlay)
              (tty-emoji-fallback-mode -1)
              (should-not (overlay-buffer overlay))
              (should (= (hash-table-count tty-emoji-fallback--windows) 0))
              (should-not pre-redisplay-functions))))
      (tty-emoji-fallback-mode -1))))

(ert-deftest tty-emoji-fallback-rejects-unsafe-placeholder ()
  (dolist (placeholder '("" "😀" "bad\ntext"))
    (let ((tty-emoji-fallback-placeholder placeholder)
          (tty-emoji-fallback-mode nil)
          (pre-redisplay-functions nil))
      (should-error (tty-emoji-fallback-mode 1) :type 'user-error)
      (should-not tty-emoji-fallback-mode)
      (should-not pre-redisplay-functions))))

(provide 'tty-emoji-fallback-tests)
;;; tty-emoji-fallback-tests.el ends here
