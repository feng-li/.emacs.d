;;; python-send-and-step.el --- Send Python groups and step -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: languages, python, processes
;; URL: https://github.com/feng-li/.emacs.d

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Elpy-independent replacements for
;; `elpy-shell-send-group-and-step' and
;; `elpy-shell-send-region-or-buffer-and-step'.  Enable
;; `python-send-and-step-mode' in Python buffers to bind:
;;
;;   C-c C-c  `python-send-and-step-group'
;;   C-c C-r  `python-send-and-step-region-or-buffer'

;;; Code:

(require 'python)

(defun python-send-and-step--line-code-p ()
  "Return non-nil when the current line contains Python code."
  (and (not (python-info-current-line-empty-p))
       (not (python-info-current-line-comment-p))))

(defun python-send-and-step--skip-to-code-line (&optional backward)
  "Move to the nearest code line, searching BACKWARD when non-nil."
  (let ((step (if backward -1 1)))
    (while (and (not (python-send-and-step--line-code-p))
                (if backward (not (bobp)) (not (eobp))))
      (forward-line step))))

(defun python-send-and-step--dedenter-line-p ()
  "Return non-nil when the current line continues a compound statement."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "\\(?:elif\\|else\\|except\\|finally\\)\\_>")))

(defun python-send-and-step--decorator-line-p ()
  "Return non-nil when the current line is a Python decorator."
  (save-excursion
    (back-to-indentation)
    (looking-at-p "@[[:alpha:]_]")))

(defun python-send-and-step--decorated-definition-line-p ()
  "Return non-nil when the current definition has a decorator."
  (save-excursion
    (python-nav-backward-statement)
    (python-send-and-step--decorator-line-p)))

(defun python-send-and-step--forward-same-indent-statement ()
  "Move to the next statement at the current indentation level."
  (let ((indent (current-indentation))
        (start (point))
        previous)
    (python-nav-forward-statement)
    (while (and (> (current-indentation) indent)
                (not (eobp)))
      (when (equal previous (point))
        (user-error "Python statement does not finish"))
      (setq previous (point))
      (python-nav-forward-statement))
    (when (< (current-indentation) indent)
      (goto-char start))
    (unless (= (point) start)
      (point))))

(defun python-send-and-step--backward-same-indent-statement ()
  "Move to the previous statement at the current indentation level."
  (let ((indent (current-indentation))
        (start (point))
        previous)
    (python-nav-backward-statement)
    (while (and (> (current-indentation) indent)
                (not (bobp)))
      (when (equal previous (point))
        (user-error "Python statement does not start"))
      (setq previous (point))
      (python-nav-backward-statement))
    (when (< (current-indentation) indent)
      (goto-char start))
    (unless (= (point) start)
      (point))))

(defun python-send-and-step--beginning-of-statement ()
  "Move to the beginning of the current or next complete statement."
  (python-send-and-step--skip-to-code-line)
  (python-nav-beginning-of-statement)
  (let (previous)
    (while (and (not (equal previous (point)))
                (or (python-send-and-step--dedenter-line-p)
                    (python-send-and-step--decorated-definition-line-p)))
      (setq previous (point))
      (python-send-and-step--backward-same-indent-statement))))

(defun python-send-and-step--end-of-statement ()
  "Move from a statement beginning to the end of the complete statement."
  (let ((continue t)
        previous)
    (while (and continue (not (equal previous (point))))
      (when (python-send-and-step--decorator-line-p)
        (python-send-and-step--forward-same-indent-statement))
      (setq previous (point))
      (python-send-and-step--forward-same-indent-statement)
      (if (equal previous (point))
          (progn
            (python-nav-end-of-block)
            (setq continue nil))
        (unless (python-send-and-step--dedenter-line-p)
          (forward-line -1)
          (python-send-and-step--skip-to-code-line t)
          (setq continue nil)))))
  (end-of-line))

(defun python-send-and-step--beginning-of-top-level-statement ()
  "Move to the beginning of the current or next top-level statement."
  (python-send-and-step--beginning-of-statement)
  (let (previous)
    (while (and (not (equal previous (point)))
                (> (current-indentation) 0))
      (setq previous (point))
      (forward-line -1)
      (python-send-and-step--skip-to-code-line t)
      (python-send-and-step--beginning-of-statement))))

(defun python-send-and-step--group-bounds ()
  "Return bounds of the current or next blank-line-separated Python group."
  (save-excursion
    (python-send-and-step--beginning-of-top-level-statement)
    (while (not (or (python-info-current-line-empty-p) (bobp)))
      (unless (python-info-current-line-comment-p)
        (python-send-and-step--beginning-of-top-level-statement))
      (forward-line -1)
      (beginning-of-line))
    (when (python-info-current-line-empty-p)
      (forward-line 1)
      (beginning-of-line))
    (let ((beg (point))
          previous)
      (unless (python-info-current-line-comment-p)
        (python-send-and-step--end-of-statement))
      (while (not (equal previous (point)))
        (setq previous (point))
        (forward-line 1)
        (if (python-info-current-line-empty-p)
            (goto-char previous)
          (unless (python-info-current-line-comment-p)
            (python-send-and-step--end-of-statement))))
      (cons beg (point)))))

;;;###autoload
(defun python-send-and-step-group ()
  "Send the current or next top-level Python group, then step forward.
A group is a sequence of top-level statements not separated by an
empty line.  Empty lines inside compound statements are ignored."
  (interactive)
  (pcase-let ((`(,beg . ,end) (python-send-and-step--group-bounds)))
    (if (<= end beg)
        (goto-char (point-max))
      (let ((end-marker (copy-marker end)))
        ;; Elpy's group command executes __main__ blocks, so SEND-MAIN is t.
        (python-shell-send-region beg end t t)
        (goto-char end-marker)
        (set-marker end-marker nil)
        (python-nav-forward-statement)
        (deactivate-mark)))))

;;;###autoload
(defun python-send-and-step-region-or-buffer (send-main)
  "Send the active region or whole buffer to Python, then step to its end.
With prefix argument SEND-MAIN, execute code guarded by
`if __name__ == \"__main__\"'."
  (interactive "P")
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end-marker (copy-marker (region-end))))
        (python-shell-send-region beg end-marker send-main t)
        (goto-char end-marker)
        (set-marker end-marker nil))
    (python-shell-send-buffer send-main t)
    (goto-char (point-max))))

(defvar-keymap python-send-and-step-mode-map
  :doc "Keymap for `python-send-and-step-mode'."
  "C-c C-c" #'python-send-and-step-group
  "C-c C-r" #'python-send-and-step-region-or-buffer)

;;;###autoload
(define-minor-mode python-send-and-step-mode
  "Use send-and-step commands in the current Python buffer."
  :lighter nil
  :keymap python-send-and-step-mode-map)

;;;###autoload
(defun turn-on-python-send-and-step-mode ()
  "Enable `python-send-and-step-mode' in the current buffer."
  (python-send-and-step-mode 1))

(provide 'python-send-and-step)
;;; python-send-and-step.el ends here
