;;; company-jinx.el --- Correct Jinx misspellings with Company -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (company "0.10.0") (jinx "1.0"))
;; Keywords: abbrev, convenience, completion
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

;; `company-jinx-correct' displays Jinx corrections in a Company popup at the
;; misspelled word under point.  The backend is started manually and therefore
;; does not interfere with ordinary Company completion.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'jinx)

(defgroup company-jinx nil
  "Correct Jinx misspellings with Company."
  :group 'company
  :group 'jinx
  :prefix "company-jinx-")

(defcustom company-jinx-auto-popup-delay 0.3
  "Seconds to wait before showing corrections for a Jinx underline at point."
  :type 'number
  :group 'company-jinx)

(defvar-local company-jinx--start-marker nil)
(defvar-local company-jinx--end-marker nil)
(defvar-local company-jinx--word nil)
(defvar-local company-jinx--candidates nil)
(defvar-local company-jinx--auto-timer nil)
(defvar-local company-jinx--auto-dismissed-state nil)

(defun company-jinx--cancel-auto-timer ()
  "Cancel the pending automatic Company Jinx popup."
  (when (timerp company-jinx--auto-timer)
    (cancel-timer company-jinx--auto-timer))
  (setq company-jinx--auto-timer nil))

(defun company-jinx--dismiss-auto-popup (&rest _)
  "Suppress automatic reopening at the current buffer position."
  (setq company-jinx--auto-dismissed-state
        (cons (point) (buffer-chars-modified-tick))))

(defun company-jinx--cleanup (&rest _)
  "Clear state left by a Company Jinx correction."
  (when (markerp company-jinx--start-marker)
    (set-marker company-jinx--start-marker nil))
  (when (markerp company-jinx--end-marker)
    (set-marker company-jinx--end-marker nil))
  (setq company-jinx--start-marker nil
        company-jinx--end-marker nil
        company-jinx--word nil
        company-jinx--candidates nil)
  (remove-hook 'company-after-completion-hook #'company-jinx--cleanup t)
  (remove-hook 'company-completion-cancelled-hook
               #'company-jinx--dismiss-auto-popup t)
  (remove-hook 'post-command-hook #'company-jinx--hide-after-leaving-word t))

(defun company-jinx--point-on-source-word-p ()
  "Return non-nil when point remains on the word being corrected."
  (and (markerp company-jinx--start-marker)
       (markerp company-jinx--end-marker)
       (eq (marker-buffer company-jinx--start-marker) (current-buffer))
       (eq (marker-buffer company-jinx--end-marker) (current-buffer))
       (<= (marker-position company-jinx--start-marker) (point))
       (<= (point) (marker-position company-jinx--end-marker))))

(defun company-jinx--hide-after-leaving-word ()
  "Hide an active Company Jinx popup after point leaves its source word."
  (when (and (eq company-backend 'company-jinx)
             company-candidates
             (not (company-jinx--point-on-source-word-p)))
    (company-abort)))

(defun company-jinx--existing-overlay-at-point ()
  "Return an existing Jinx misspelling overlay at point."
  (let ((origin (point)))
    (cl-find-if
     (lambda (overlay)
       (and (eq (overlay-get overlay 'category) 'jinx-overlay)
            (<= (overlay-start overlay) origin)
            (<= origin (overlay-end overlay))))
     (append (overlays-at origin)
             (when (> origin (point-min))
               (overlays-at (1- origin)))))))

(defun company-jinx--overlay-at-point ()
  "Return the Jinx misspelling overlay for the word at point."
  (let ((origin (point))
        (bounds (jinx--bounds-of-word)))
    (unless (and bounds
                 (<= (car bounds) origin)
                 (<= origin (cdr bounds)))
      (user-error "Point is not on a word"))
    (or (company-jinx--existing-overlay-at-point)
        (car (jinx--force-overlays (car bounds) (cdr bounds) :check t)))))

(defun company-jinx--save-action (candidate)
  "Return the Jinx save action encoded by CANDIDATE, or nil."
  (and (> (length candidate) 0)
       (assq (aref candidate 0) jinx--save-keys)))

(defun company-jinx--company-candidate (candidate word)
  "Convert Jinx CANDIDATE for WORD into a Company candidate."
  (if (company-jinx--save-action candidate)
      (let ((display (copy-sequence word)))
        (add-text-properties
         0 (length display)
         `(company-jinx--save-choice ,(substring-no-properties candidate)
           company-jinx--annotation ,(get-text-property 0 'jinx--suffix candidate)
           company-jinx--group ,(get-text-property 0 'jinx--group candidate))
         display)
        display)
    candidate))

(defun company-jinx--post-completion (candidate)
  "Apply any Jinx save action attached to CANDIDATE."
  (when-let* ((choice (get-text-property 0 'company-jinx--save-choice candidate))
              (action (company-jinx--save-action choice)))
    (let ((key (car action))
          (function (cdr action)))
      (funcall function 'add key
               (if (> (length choice) 1)
                   (substring-no-properties choice 1)
                 company-jinx--word))
      (jinx--recheck-overlays))))

;;;###autoload
(defun company-jinx (command &optional arg &rest rest)
  "Company backend providing Jinx corrections.

COMMAND, ARG, and remaining arguments follow the Company backend protocol."
  (interactive (list 'interactive))
  (pcase command
    ('interactive (company-jinx-correct))
    ('prefix
     (when (company-jinx--point-on-source-word-p)
       (let ((start (marker-position company-jinx--start-marker))
             (end (marker-position company-jinx--end-marker)))
         (list (buffer-substring-no-properties start (point))
               (buffer-substring-no-properties (point) end)
               t))))
    ('candidates company-jinx--candidates)
    ('adjust-boundaries (cons (car rest) (cadr rest)))
    ('annotation (get-text-property 0 'company-jinx--annotation arg))
    ('meta (get-text-property 0 'company-jinx--group arg))
    ('kind 'text)
    ('sorted t)
    ('duplicates t)
    ('no-cache t)
    ('require-match t)
    ('post-completion (company-jinx--post-completion arg))))

(defun company-jinx--start (overlay)
  "Start Company correction for Jinx misspelling OVERLAY."
  (when company-candidates
    (company-abort))
  (company-jinx--cancel-auto-timer)
  (company-jinx--cleanup)
  (let* ((start (overlay-start overlay))
         (end (overlay-end overlay))
         (word (buffer-substring-no-properties start end))
         (suggestions (jinx--correct-suggestions word)))
    (setq company-jinx--start-marker (copy-marker start)
          company-jinx--end-marker (copy-marker end t)
          company-jinx--word word
          company-jinx--candidates
          (mapcar (lambda (candidate)
                    (company-jinx--company-candidate candidate word))
                  suggestions))
    (unless company-jinx--candidates
      (company-jinx--cleanup)
      (user-error "Jinx has no suggestions for `%s'" word))
    (unless company-mode
      (company-mode 1))
    (add-hook 'company-after-completion-hook #'company-jinx--cleanup nil t)
    (add-hook 'company-completion-cancelled-hook
              #'company-jinx--dismiss-auto-popup nil t)
    (add-hook 'post-command-hook #'company-jinx--hide-after-leaving-word nil t)
    (condition-case err
        (company-begin-backend 'company-jinx)
      (error
       (company-jinx--cleanup)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun company-jinx-correct ()
  "Show Jinx corrections in a Company popup for the word at point."
  (interactive)
  (jinx--correct-guard
   (company-jinx--start (company-jinx--overlay-at-point))))

(defun company-jinx--auto-show (buffer position tick)
  "Show corrections in BUFFER at POSITION if it is still at TICK."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq company-jinx--auto-timer nil)
      (when (and company-jinx-auto-popup-mode
                 jinx-mode
                 (eq (window-buffer (selected-window)) buffer)
                 (= (point) position)
                 (= (buffer-chars-modified-tick) tick)
                 (not company-candidates)
                 (not (equal company-jinx--auto-dismissed-state
                             (cons position tick))))
        (when-let* ((overlay (company-jinx--existing-overlay-at-point)))
          (condition-case err
              (jinx--correct-guard
               (company-jinx--start overlay))
            (error
             (message "Company-Jinx: %s" (error-message-string err)))))))))

(defun company-jinx--auto-post-command ()
  "Schedule a popup when point rests on a Jinx misspelling."
  (company-jinx--cancel-auto-timer)
  (let ((state (cons (point) (buffer-chars-modified-tick))))
    (unless (equal state company-jinx--auto-dismissed-state)
      (setq company-jinx--auto-dismissed-state nil))
    (when (and company-jinx-auto-popup-mode
               jinx-mode
               (not company-candidates)
               (not (equal state company-jinx--auto-dismissed-state))
               (company-jinx--existing-overlay-at-point))
      (setq company-jinx--auto-timer
            (run-with-idle-timer
             company-jinx-auto-popup-delay nil
             #'company-jinx--auto-show (current-buffer)
             (point) (buffer-chars-modified-tick))))))

;;;###autoload
(define-minor-mode company-jinx-auto-popup-mode
  "Automatically show Company corrections on a Jinx underline at point."
  :lighter nil
  :group 'company-jinx
  (if company-jinx-auto-popup-mode
      (add-hook 'post-command-hook #'company-jinx--auto-post-command nil t)
    (remove-hook 'post-command-hook #'company-jinx--auto-post-command t)
    (company-jinx--cancel-auto-timer)
    (setq company-jinx--auto-dismissed-state nil)))

;;;###autoload
(defun company-jinx-auto-popup-setup ()
  "Enable automatic popups exactly when `jinx-mode' is enabled."
  (company-jinx-auto-popup-mode (if jinx-mode 1 -1)))

(provide 'company-jinx)

;;; company-jinx.el ends here
