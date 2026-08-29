;;; company-numbered-selection.el --- Select Company candidates with bare keys -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (company "0.10.0"))
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

;; This package makes unmodified number keys select the corresponding visible
;; Company candidate.  A number is inserted normally when it can extend the
;; current completion prefix, so candidates such as "sha256" remain typeable.
;; The same distinction is applied to incremental Company searches.
;;
;; Enable the behavior globally with:
;;
;;   (company-numbered-selection-mode 1)
;;
;; The default keys select rows 1 through 10, with 0 selecting row 10.  Change
;; `company-numbered-selection-keys' before enabling the mode to use another
;; ordered set of single-character keys.

;;; Code:

(require 'company)
(require 'seq)

(defgroup company-numbered-selection nil
  "Select visible Company candidates with unmodified keys."
  :group 'company
  :prefix "company-numbered-selection-")

(defvar company-numbered-selection-mode)
(defvar company-numbered-selection--saved-bindings nil)
(defvar company-numbered-selection--saved-options nil)

(defun company-numbered-selection--valid-key-p (key)
  "Return non-nil when KEY describes one unmodified character event."
  (and (stringp key)
       (= (length key) 1)
       (condition-case nil
           (let ((events (kbd key)))
             (and (= (length events) 1)
                  (characterp (aref events 0))))
         (error nil))))

(defun company-numbered-selection--set-keys (symbol value)
  "Set SYMBOL to VALUE and refresh active Company key bindings."
  (unless (and (listp value)
               value
               (seq-every-p #'company-numbered-selection--valid-key-p value)
               (= (length value) (length (delete-dups (copy-sequence value)))))
    (user-error "Numbered-selection keys must be unique single characters"))
  (let ((enabled (bound-and-true-p company-numbered-selection-mode)))
    (when enabled
      (company-numbered-selection--restore-key-bindings))
    (set-default symbol value)
    (when enabled
      (company-numbered-selection--install-key-bindings))))

(defcustom company-numbered-selection-keys
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
  "Keys used to select the corresponding visible Company rows.
The first key selects row one.  Every value must describe one unmodified
character event."
  :type '(repeat string)
  :set #'company-numbered-selection--set-keys
  :group 'company-numbered-selection)

(defcustom company-numbered-selection-show-hints t
  "Whether to show selection keys beside Company candidates."
  :type 'boolean
  :group 'company-numbered-selection)

(defun company-numbered-selection--event-key ()
  "Return `last-command-event' as a one-character string."
  (let ((event (event-basic-type last-command-event)))
    (if (characterp event)
        (char-to-string event)
      (user-error "Numbered selection requires a character key"))))

(defun company-numbered-selection--prefix-extends-p (key)
  "Return non-nil when inserting KEY can extend a current candidate."
  (when (stringp company-prefix)
    (let ((next-prefix (concat company-prefix key))
          (ignore-case (and company-backend
                            (company-call-backend 'ignore-case))))
      (seq-some
       (lambda (candidate)
         (and (stringp candidate)
              (string-prefix-p next-prefix candidate ignore-case)))
       company-candidates))))

(defun company-numbered-selection--search-extends-p (key)
  "Return non-nil when KEY can extend the active Company search."
  (let ((regexp
         (funcall company-search-regexp-function
                  (concat company-search-string key))))
    (seq-some
     (lambda (candidate)
       (and (stringp candidate) (string-match-p regexp candidate)))
     company-candidates)))

(defun company-numbered-selection--can-insert-p (key)
  "Return non-nil when KEY should refine completion instead of select."
  (if (bound-and-true-p company-search-mode)
      (company-numbered-selection--search-extends-p key)
    (company-numbered-selection--prefix-extends-p key)))

;;;###autoload
(defun company-numbered-selection-select-or-insert ()
  "Select the row denoted by the pressed key, or insert that key.
Insert the key when doing so can still match a Company candidate.  Otherwise,
complete the corresponding visible tooltip row."
  (interactive)
  (let* ((key (company-numbered-selection--event-key))
         (row (seq-position company-numbered-selection-keys key #'equal)))
    (unless row
      (user-error "Key %s is not configured for numbered selection" key))
    (if (company-numbered-selection--can-insert-p key)
        (if (bound-and-true-p company-search-mode)
            (company-search-printing-char)
          (self-insert-command 1))
      (company-complete-tooltip-row (1+ row)))))

(defun company-numbered-selection--hint (row)
  "Return the configured selection hint for zero-based ROW."
  (or (nth row company-numbered-selection-keys) ""))

(defun company-numbered-selection--install-key-bindings ()
  "Install numbered selection in Company's active keymaps."
  (setq company-numbered-selection--saved-bindings nil)
  (dolist (map (list company-active-map company-search-map))
    (dolist (key company-numbered-selection-keys)
      (let ((sequence (kbd key)))
        (push (list map sequence (lookup-key map sequence))
              company-numbered-selection--saved-bindings)
        (define-key map sequence
                    #'company-numbered-selection-select-or-insert)))))

(defun company-numbered-selection--restore-key-bindings ()
  "Restore Company bindings replaced by numbered selection."
  (dolist (entry company-numbered-selection--saved-bindings)
    (let ((map (nth 0 entry))
          (sequence (nth 1 entry))
          (binding (nth 2 entry)))
      (when (eq (lookup-key map sequence)
                #'company-numbered-selection-select-or-insert)
        (define-key map sequence binding))))
  (setq company-numbered-selection--saved-bindings nil))

(defun company-numbered-selection--enable ()
  "Enable numbered Company candidate selection."
  (unless company-numbered-selection--saved-bindings
    (company-numbered-selection--install-key-bindings)
    (when company-numbered-selection-show-hints
      (setq company-numbered-selection--saved-options
            (list company-show-quick-access
                  company-quick-access-hint-function)
            company-show-quick-access t
            company-quick-access-hint-function
            #'company-numbered-selection--hint))))

(defun company-numbered-selection--disable ()
  "Disable numbered Company candidate selection."
  (company-numbered-selection--restore-key-bindings)
  (when company-numbered-selection--saved-options
    (when (eq company-quick-access-hint-function
              #'company-numbered-selection--hint)
      (setq company-quick-access-hint-function
            (nth 1 company-numbered-selection--saved-options)))
    (when company-show-quick-access
      (setq company-show-quick-access
            (nth 0 company-numbered-selection--saved-options)))
    (setq company-numbered-selection--saved-options nil)))

;;;###autoload
(define-minor-mode company-numbered-selection-mode
  "Toggle unmodified-key selection of visible Company candidates."
  :global t
  :group 'company-numbered-selection
  (if company-numbered-selection-mode
      (company-numbered-selection--enable)
    (company-numbered-selection--disable)))

(defun company-numbered-selection-unload-function ()
  "Disable numbered selection before unloading this package."
  (company-numbered-selection-mode -1)
  nil)

(provide 'company-numbered-selection)

;;; company-numbered-selection.el ends here
