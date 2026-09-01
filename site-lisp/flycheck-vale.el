;;; flycheck-vale.el --- Flycheck integration for Vale  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (flycheck "32"))
;; Keywords: convenience, text, tools
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

;; This package adds a Flycheck checker backed by the Vale prose linter.
;; Checks use the current buffer contents through standard input, so unsaved
;; edits are included.  Vale's JSON output is converted to Flycheck errors
;; while preserving error, warning, and suggestion severity.
;;
;; Enable the checker with:
;;
;;   (require 'flycheck-vale)
;;   (flycheck-vale-setup)
;;
;; To run Vale after another prose checker, add a Flycheck checker chain:
;;
;;   (flycheck-add-next-checker 'languagetool 'vale)
;;
;; Vale does not parse LaTeX markup natively.  In `latex-mode' and
;; `LaTeX-mode', this package therefore passes `--ignore-syntax'; phrase rules
;; work, but TeX commands can produce false positives.

;;; Code:

(require 'cl-lib)
(require 'flycheck)
(require 'json)

(defgroup flycheck-vale nil
  "Flycheck support for the Vale prose linter."
  :group 'flycheck
  :prefix "flycheck-vale-")

(flycheck-def-executable-var vale "vale")

(defcustom flycheck-vale-args nil
  "Additional command-line arguments passed to Vale.

Each element must be one complete argument.  These arguments precede the
automatically selected `--ext' and `--ignore-syntax' arguments."
  :type '(repeat string)
  :group 'flycheck-vale)

(defcustom flycheck-vale-mode-extensions
  '((markdown-mode . "md")
    (gfm-mode . "md")
    (org-mode . "org")
    (LaTeX-mode . "tex")
    (latex-mode . "tex")
    (text-mode . "txt"))
  "Alist mapping major modes to extensions understood by Vale.

An entry also applies to modes derived from its key.  Put more specific modes
before their parent modes.  Extensions must omit the leading period."
  :type '(alist :key-type symbol :value-type string)
  :group 'flycheck-vale)

(defcustom flycheck-vale-ignore-syntax-modes
  '(LaTeX-mode latex-mode)
  "Modes in which Vale should lint input line by line.

The `--ignore-syntax' option is used for these modes.  An entry also applies to
modes derived from it."
  :type '(repeat symbol)
  :group 'flycheck-vale)

(defun flycheck-vale--set-modes (symbol value)
  "Set SYMBOL to VALUE and update an existing Vale checker."
  (set-default symbol value)
  (when (flycheck-valid-checker-p 'vale)
    (setf (flycheck-checker-get 'vale 'modes) value)))

(defcustom flycheck-vale-modes
  '(text-mode markdown-mode gfm-mode org-mode latex-mode LaTeX-mode)
  "Major modes supported by the Vale checker."
  :type '(repeat symbol)
  :set #'flycheck-vale--set-modes
  :group 'flycheck-vale)

(defun flycheck-vale--derived-mode-p (mode)
  "Return non-nil when the current major mode derives from MODE."
  (or (eq major-mode mode)
      (derived-mode-p mode)))

(defun flycheck-vale--extension ()
  "Return the Vale input extension appropriate for the current buffer."
  (or (cl-loop for (mode . extension) in flycheck-vale-mode-extensions
               when (flycheck-vale--derived-mode-p mode)
               return extension)
      "txt"))

(defun flycheck-vale--ignore-syntax-p ()
  "Return non-nil when Vale should ignore syntax in the current buffer."
  (cl-some #'flycheck-vale--derived-mode-p
           flycheck-vale-ignore-syntax-modes))

(defun flycheck-vale--arguments ()
  "Return buffer-specific command-line arguments for Vale."
  (append flycheck-vale-args
          (list (concat "--ext=." (flycheck-vale--extension)))
          (when (flycheck-vale--ignore-syntax-p)
            '("--ignore-syntax"))))

(defun flycheck-vale--level (severity)
  "Translate Vale SEVERITY to a Flycheck error level."
  (pcase (downcase (or severity ""))
    ("error" 'error)
    ("warning" 'warning)
    (_ 'info)))

(defun flycheck-vale--runtime-error (data checker buffer)
  "Convert runtime error DATA into an error for CHECKER and BUFFER."
  (when-let* ((message (alist-get 'Text data)))
    (flycheck-error-new-at
     1 1 'error message
     :checker checker
     :id (alist-get 'Code data)
     :buffer buffer
     :filename (buffer-file-name buffer))))

(defun flycheck-vale--alert-error (alert checker buffer)
  "Convert one Vale ALERT into an error for CHECKER and BUFFER."
  (let* ((line (or (alist-get 'Line alert) 1))
         (span (alist-get 'Span alert))
         (column (or (car-safe span) 1))
         (last-column (cadr span)))
    (flycheck-error-new-at
     line column
     (flycheck-vale--level (alist-get 'Severity alert))
     (or (alist-get 'Message alert) "Vale reported a problem")
     :end-line (and last-column line)
     ;; Vale's final span column is inclusive; Flycheck's is right-open.
     :end-column (and last-column (1+ last-column))
     :checker checker
     :id (alist-get 'Check alert)
     :buffer buffer
     :filename (buffer-file-name buffer))))

(defun flycheck-vale--parse (output checker buffer)
  "Parse Vale JSON OUTPUT for CHECKER in BUFFER.

Return a list of `flycheck-error' objects."
  (let ((data
         (condition-case nil
             (json-parse-string
              output
              :object-type 'alist
              :array-type 'list
              :null-object nil
              :false-object nil)
           (json-parse-error nil))))
    (cond
     ((null data) nil)
     ((alist-get 'Code data)
      (when-let* ((error
                   (flycheck-vale--runtime-error data checker buffer)))
        (list error)))
     (t
      (cl-loop for (_file . alerts) in data
               append (mapcar
                       (lambda (alert)
                         (flycheck-vale--alert-error
                          alert checker buffer))
                       alerts))))))

(flycheck-define-command-checker 'vale
  "Check prose with Vale.

See URL `https://vale.sh/'."
  :command
  '("vale" "--output=JSON" "--no-exit"
    (eval (flycheck-vale--arguments)))
  :standard-input t
  :error-parser #'flycheck-vale--parse
  :modes flycheck-vale-modes)

;;;###autoload
(defun flycheck-vale-setup ()
  "Register the Vale checker with Flycheck."
  (interactive)
  ;; Append so an existing primary checker remains primary and can chain to
  ;; Vale explicitly with `flycheck-add-next-checker'.
  (add-to-list 'flycheck-checkers 'vale t))

(defun flycheck-vale-unload-function ()
  "Remove the Vale checker before unloading this package."
  (setq flycheck-checkers (delq 'vale flycheck-checkers))
  nil)

(provide 'flycheck-vale)

;;; flycheck-vale.el ends here
