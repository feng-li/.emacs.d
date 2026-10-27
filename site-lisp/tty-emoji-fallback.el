;;; tty-emoji-fallback.el --- Display-only safe emoji for TTY windows -*- lexical-binding: t; -*-

;;; Commentary:
;; Replace narrow emoji and compound emoji with ASCII in terminal windows.
;; Use window-specific overlays: files, buffer text, and GUI windows are intact.
;; Emoji bases come from Emacs's bundled Unicode data, not a glyph blacklist.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup tty-emoji-fallback nil
  "Plain-text fallback for terminal emoji width disagreements."
  :group 'display)

(defcustom tty-emoji-fallback-placeholder "[emoji]"
  "ASCII text to display instead of narrow or compound emoji.
Ordinary standalone two-column emoji remain visible.  Re-enable
`tty-emoji-fallback-mode' after changing this option."
  :type 'string
  :group 'tty-emoji-fallback)

(defvar emoji--names)
(defvar tty-emoji-fallback--regexp nil
  "Cached emoji-sequence matcher built from Emacs's Unicode emoji names.")
(defvar tty-emoji-fallback--windows (make-hash-table :test #'eq)
  "Per-window cache of the last scanned range and its display overlays.")
(defvar tty-emoji-fallback-mode)

(defun tty-emoji-fallback--regexp ()
  "Recognize emoji bases and Unicode-standard compound sequence syntax."
  (or tty-emoji-fallback--regexp
      (progn
        (require 'emoji-labels)
        (let ((bases (make-hash-table :test #'eql)))
          (maphash
           (lambda (glyph _name)
             (seq-doseq (char glyph)
               (when (and (> char 127)
                          (not (<= #x1f3fb char #x1f3ff))
                          (not (memq (get-char-code-property char 'general-category)
                                     '(Cf Mn Me))))
                 (puthash char t bases))))
           emoji--names)
          (let* ((base (regexp-opt (mapcar #'char-to-string (hash-table-keys bases))))
                 (component (concat base "[\ufe0e\ufe0f]?[\U0001f3fb-\U0001f3ff]?")))
            (setq tty-emoji-fallback--regexp
                  (concat
                   "\\(?:"
                   ;; Country flags, subdivision flags, and keycaps precede
                   ;; the general expression so a complete sequence wins.
                   "[\U0001f1e6-\U0001f1ff]\\{2\\}\\|"
                   "\U0001f3f4[\U000e0020-\U000e007e]+\U000e007f\\|"
                   "[#*0-9][\ufe0e\ufe0f]?\u20e3\\|"
                   component "\\(?:\u200d" component "\\)*"
                   "\\)")))))))

(defun tty-emoji-fallback--clear-window (window)
  "Remove only fallback overlays owned by WINDOW."
  (mapc #'delete-overlay (cdr (gethash window tty-emoji-fallback--windows)))
  (remhash window tty-emoji-fallback--windows))

(defun tty-emoji-fallback--prune ()
  "Release overlays belonging to deleted windows or replaced buffers."
  (let (stale)
    (maphash (lambda (window state)
               (unless (and (window-live-p window)
                            (eq (window-buffer window) (caar state)))
                 (push window stale)))
             tty-emoji-fallback--windows)
    (mapc #'tty-emoji-fallback--clear-window stale)))

(defun tty-emoji-fallback--redisplay (window)
  "Prepare safe emoji display in terminal WINDOW before it is drawn.
Only scan visible logical lines, and cache unchanged content/ranges.
No buffer text, text properties, display tables, or character widths change."
  (when (and tty-emoji-fallback-mode (window-live-p window))
    (tty-emoji-fallback--prune)
    (if (display-graphic-p (window-frame window))
        (tty-emoji-fallback--clear-window window)
      (with-current-buffer (window-buffer window)
        (save-excursion
          (save-match-data
            (let* ((start (progn (goto-char (window-start window))
                                 (line-beginning-position)))
                   (end (progn (goto-char (or (window-end window t) (point-max)))
                               (line-end-position)))
                   (key (list (current-buffer) (buffer-modified-tick)
                              (point-min) (point-max) start end
                              tty-emoji-fallback-placeholder))
                   (state (gethash window tty-emoji-fallback--windows)))
              (unless (equal key (car state))
                (tty-emoji-fallback--clear-window window)
                (goto-char start)
                (let (overlays)
                  (while (re-search-forward (tty-emoji-fallback--regexp) end t)
                    (let ((glyph (match-string-no-properties 0))
                          (beg (match-beginning 0))
                          (finish (match-end 0)))
                      (when (and (or (> (length glyph) 1)
                                     (/= (char-width (aref glyph 0)) 2))
                                 ;; Respect folded text and existing replacing
                                 ;; display specs, e.g. images or prettification.
                                 (not (invisible-p beg))
                                 (not (get-char-property beg 'display window)))
                        (let ((overlay (make-overlay beg finish)))
                          (overlay-put overlay 'tty-emoji-fallback t)
                          (overlay-put overlay 'window window)
                          (overlay-put overlay 'display
                                       (copy-sequence tty-emoji-fallback-placeholder))
                          (overlay-put overlay 'evaporate t)
                          (push overlay overlays)))))
                  (puthash window (cons key overlays)
                           tty-emoji-fallback--windows))))))))))

;;;###autoload
(define-minor-mode tty-emoji-fallback-mode
  "Display narrow and compound emoji as ASCII in terminal Emacs windows.
GUI windows and stored text are unchanged, including when the same buffer
is displayed in both kinds of frame.  This also covers new daemon clients.
Disable this global mode to immediately restore original rendering."
  :global t
  :group 'tty-emoji-fallback
  (if tty-emoji-fallback-mode
      (progn
        (unless (and (stringp tty-emoji-fallback-placeholder)
                     (string-match-p "\\`[ -~]+\\'" tty-emoji-fallback-placeholder))
          (setq tty-emoji-fallback-mode nil)
          (user-error "Emoji placeholder must be nonempty, printable ASCII"))
        (add-hook 'pre-redisplay-functions #'tty-emoji-fallback--redisplay)
        (dolist (window (window-list-1 nil t t))
          (tty-emoji-fallback--redisplay window)))
    (remove-hook 'pre-redisplay-functions #'tty-emoji-fallback--redisplay)
    (let (windows)
      (maphash (lambda (window _state) (push window windows))
               tty-emoji-fallback--windows)
      (mapc #'tty-emoji-fallback--clear-window windows)))
  (force-window-update t))

(provide 'tty-emoji-fallback)
;;; tty-emoji-fallback.el ends here
