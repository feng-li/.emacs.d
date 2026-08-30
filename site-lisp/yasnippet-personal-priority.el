;;; yasnippet-personal-priority.el --- Prefer personal snippets by trigger -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (yasnippet "0.14.0"))
;; Keywords: convenience, snippets
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

;; Yasnippet directory ordering resolves snippets with the same identity, but
;; snippets with different names can still expose the same trigger.  This
;; package removes non-personal alternatives whenever a personal snippet uses
;; that trigger.  The rule applies to direct expansion, Yasnippet menus, and,
;; when available, company-yasnippet candidates.
;;
;; Enable the behavior globally with:
;;
;;   (yasnippet-personal-priority-mode 1)
;;
;; By default, snippets below USER-EMACS-DIRECTORY/snippets are personal.
;; Customize `yasnippet-personal-priority-directories' to recognize additional
;; directories.
;;
;; This package necessarily advises private Yasnippet and company-yasnippet
;; functions because their public APIs do not expose the candidate lists at the
;; required stage.  The advice is installed idempotently and removed when the
;; mode is disabled or the package is unloaded.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'yasnippet)

(defgroup yasnippet-personal-priority nil
  "Prefer personal Yasnippet definitions that share trigger keys."
  :group 'yasnippet
  :prefix "yasnippet-personal-priority-")

(defcustom yasnippet-personal-priority-directories
  (list (expand-file-name "snippets" user-emacs-directory))
  "Directories whose Yasnippet templates take priority by trigger key."
  :type '(repeat directory)
  :group 'yasnippet-personal-priority)

(defconst yasnippet-personal-priority--cache-miss
  (make-symbol "yasnippet-personal-priority-cache-miss"))

(defvar yasnippet-personal-priority--cache-directories nil)

(defvar yasnippet-personal-priority--template-origin-cache
  (make-hash-table :test #'eq :weakness 'key)
  "Cached personal status keyed by Yasnippet template object.")

;;;###autoload
(defun yasnippet-personal-priority-clear-cache ()
  "Clear cached personal-directory classifications."
  (interactive)
  (clrhash yasnippet-personal-priority--template-origin-cache)
  (setq yasnippet-personal-priority--cache-directories
        (copy-tree yasnippet-personal-priority-directories))
  (when (called-interactively-p 'interactive)
    (message "Yasnippet personal-priority cache cleared")))

(defun yasnippet-personal-priority--ensure-current-cache ()
  "Invalidate cached classifications after a directory change."
  (unless (equal yasnippet-personal-priority-directories
                 yasnippet-personal-priority--cache-directories)
    (yasnippet-personal-priority-clear-cache)))

(defun yasnippet-personal-priority--personal-template-p (template)
  "Return non-nil when TEMPLATE belongs to a priority directory."
  (yasnippet-personal-priority--ensure-current-cache)
  (let ((cached
         (gethash template
                  yasnippet-personal-priority--template-origin-cache
                  yasnippet-personal-priority--cache-miss)))
    (if (not (eq cached yasnippet-personal-priority--cache-miss))
        cached
      (let ((personal
             (when-let* ((file (yas--template-get-file template)))
               (seq-some
                (lambda (directory)
                  (condition-case nil
                      (file-in-directory-p file (expand-file-name directory))
                    (file-error nil)))
                yasnippet-personal-priority-directories))))
        (puthash template (and personal t)
                 yasnippet-personal-priority--template-origin-cache)
        personal))))

(defun yasnippet-personal-priority--filter-items (items template-function)
  "Remove shadowed ITEMS using TEMPLATE-FUNCTION to extract templates.
A non-personal item is removed when a personal item has the same trigger key.
The original order is preserved, and all personal definitions are retained."
  (let ((personal-keys (make-hash-table :test #'equal))
        records)
    (dolist (item items)
      (let* ((template (funcall template-function item))
             (key (and template (yas--template-key template)))
             (personal (and template
                            (yasnippet-personal-priority--personal-template-p
                             template))))
        (when (and key personal)
          (puthash key t personal-keys))
        (push (list item key personal) records)))
    (if (zerop (hash-table-count personal-keys))
        items
      (let (filtered)
        (dolist (record records filtered)
          (let ((item (nth 0 record))
                (key (nth 1 record))
                (personal (nth 2 record)))
            (unless (and key (gethash key personal-keys) (not personal))
              (push item filtered))))))))

(defun yasnippet-personal-priority--filter-expansion-arguments (arguments)
  "Filter the template alist in Yasnippet expansion ARGUMENTS."
  (cons (yasnippet-personal-priority--filter-items
         (car arguments) #'cdr)
        (cdr arguments)))

(defun yasnippet-personal-priority--filter-all-templates (templates)
  "Remove bundled TEMPLATES shadowed by personal trigger keys."
  (yasnippet-personal-priority--filter-items templates #'identity))

(defun yasnippet-personal-priority--company-candidate-template (candidate)
  "Return the Yasnippet template attached to Company CANDIDATE."
  (and (stringp candidate)
       (> (length candidate) 0)
       (get-text-property 0 'yas-template candidate)))

(defun yasnippet-personal-priority--filter-company-candidates (candidates)
  "Remove Company CANDIDATES shadowed by personal snippet triggers."
  (yasnippet-personal-priority--filter-items
   candidates #'yasnippet-personal-priority--company-candidate-template))

(defun yasnippet-personal-priority--add-yasnippet-advice ()
  "Install the priority filters on Yasnippet candidate paths."
  (unless (advice-member-p
           #'yasnippet-personal-priority--filter-expansion-arguments
           'yas--expand-or-prompt-for-template)
    (advice-add 'yas--expand-or-prompt-for-template
                :filter-args
                #'yasnippet-personal-priority--filter-expansion-arguments))
  (unless (advice-member-p
           #'yasnippet-personal-priority--filter-all-templates
           'yas--all-templates)
    (advice-add 'yas--all-templates
                :filter-return
                #'yasnippet-personal-priority--filter-all-templates)))

(defun yasnippet-personal-priority--remove-yasnippet-advice ()
  "Remove the priority filters from Yasnippet candidate paths."
  (advice-remove 'yas--expand-or-prompt-for-template
                 #'yasnippet-personal-priority--filter-expansion-arguments)
  (advice-remove 'yas--all-templates
                 #'yasnippet-personal-priority--filter-all-templates))

(defun yasnippet-personal-priority--add-company-advice ()
  "Install the priority filter for company-yasnippet when available."
  (when (and (fboundp 'company-yasnippet--completions-for-prefix)
             (not (advice-member-p
                   #'yasnippet-personal-priority--filter-company-candidates
                   'company-yasnippet--completions-for-prefix)))
    (advice-add 'company-yasnippet--completions-for-prefix
                :filter-return
                #'yasnippet-personal-priority--filter-company-candidates)))

(defun yasnippet-personal-priority--remove-company-advice ()
  "Remove the priority filter from company-yasnippet."
  (when (fboundp 'company-yasnippet--completions-for-prefix)
    (advice-remove 'company-yasnippet--completions-for-prefix
                   #'yasnippet-personal-priority--filter-company-candidates)))

(defun yasnippet-personal-priority--after-load (_file)
  "Attach optional Company integration after its FILE has loaded."
  (when (featurep 'company-yasnippet)
    (yasnippet-personal-priority--add-company-advice)
    (remove-hook 'after-load-functions
                 #'yasnippet-personal-priority--after-load)))

(defun yasnippet-personal-priority--enable ()
  "Enable personal snippet priority filters."
  (yasnippet-personal-priority--add-yasnippet-advice)
  (if (featurep 'company-yasnippet)
      (yasnippet-personal-priority--add-company-advice)
    (add-hook 'after-load-functions
              #'yasnippet-personal-priority--after-load)))

(defun yasnippet-personal-priority--disable ()
  "Disable personal snippet priority filters."
  (yasnippet-personal-priority--remove-yasnippet-advice)
  (yasnippet-personal-priority--remove-company-advice)
  (remove-hook 'after-load-functions
               #'yasnippet-personal-priority--after-load))

;;;###autoload
(define-minor-mode yasnippet-personal-priority-mode
  "Toggle personal Yasnippet priority by trigger key."
  :global t
  :group 'yasnippet-personal-priority
  (if yasnippet-personal-priority-mode
      (yasnippet-personal-priority--enable)
    (yasnippet-personal-priority--disable)))

(defun yasnippet-personal-priority-unload-function ()
  "Disable personal snippet priority before unloading this package."
  (yasnippet-personal-priority-mode -1)
  nil)

(provide 'yasnippet-personal-priority)

;;; yasnippet-personal-priority.el ends here
