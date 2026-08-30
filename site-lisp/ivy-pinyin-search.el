;;; ivy-pinyin-search.el --- Pinyin matching for Ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ivy "0.15.0"))
;; Keywords: convenience, matching, multilingual
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

;; Simple Latin input automatically gains pinyin matching when Swiper's source
;; buffer or a static Ivy collection contains Chinese text.  For example,
;; `bj', `beijing', and `bei jing' all match "北京".  Append
;; `ivy-pinyin-search-suffix' to search Chinese matches only, including when the
;; source cannot be inspected.  The pinyin table comes from Emacs's built-in
;; `chinese-py' input method.
;;
;; Enable the integration globally with:
;;
;;   (ivy-pinyin-search-mode 1)

;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'quail)
(require 'subr-x)

(defgroup ivy-pinyin-search nil
  "Pinyin matching for Ivy."
  :group 'ivy
  :prefix "ivy-pinyin-search-")

(defcustom ivy-pinyin-search-suffix "`"
  "Suffix which forces Chinese-only pinyin matching."
  :type 'string)

(defcustom ivy-pinyin-search-auto-detect t
  "Whether simple Latin input uses pinyin when an Ivy source contains Chinese."
  :type 'boolean)

(defvar ivy-pinyin-search--syllables nil)
(defvar ivy-pinyin-search--initials nil)
(defvar ivy-pinyin-search--maximum-syllable-length 0)
(defvar ivy-pinyin-search--fallback-builder nil)
(defvar ivy-pinyin-search--had-default-builder nil)
(defvar ivy-pinyin-search--cached-collection nil)
(defvar ivy-pinyin-search--cached-collection-result nil)

(defvar-local ivy-pinyin-search--buffer-cache-tick nil)
(defvar-local ivy-pinyin-search--buffer-cache-result nil)

(defun ivy-pinyin-search--string-has-chinese-p (string)
  "Return non-nil when STRING contains a Chinese character."
  (and (stringp string) (string-match-p "\\cc" string)))

(defun ivy-pinyin-search--buffer-has-chinese-p (buffer)
  "Return non-nil when BUFFER contains Chinese text, using a change cache."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((tick (buffer-chars-modified-tick)))
        (unless (eql tick ivy-pinyin-search--buffer-cache-tick)
          (setq ivy-pinyin-search--buffer-cache-tick tick
                ivy-pinyin-search--buffer-cache-result
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (and (re-search-forward "\\cc" nil t) t)))))
        ivy-pinyin-search--buffer-cache-result))))

(defun ivy-pinyin-search--candidate-has-chinese-p (candidate)
  "Return non-nil when Ivy CANDIDATE contains Chinese text."
  (cond
   ((stringp candidate)
    (ivy-pinyin-search--string-has-chinese-p candidate))
   ((consp candidate)
    (or (ivy-pinyin-search--string-has-chinese-p (car candidate))
        (ivy-pinyin-search--string-has-chinese-p (cdr candidate))))))

(defun ivy-pinyin-search--collection-has-chinese-p (collection)
  "Return non-nil when static Ivy COLLECTION contains Chinese text."
  (unless (eq collection ivy-pinyin-search--cached-collection)
    (setq ivy-pinyin-search--cached-collection collection
          ivy-pinyin-search--cached-collection-result
          (cl-some #'ivy-pinyin-search--candidate-has-chinese-p collection)))
  ivy-pinyin-search--cached-collection-result)

(defun ivy-pinyin-search--source-has-chinese-p ()
  "Return non-nil when the current Ivy source is known to contain Chinese."
  (let ((caller (ivy-state-caller ivy-last))
        (collection (ivy-state-collection ivy-last)))
    (cond
     ((memq caller '(swiper swiper-isearch))
      (ivy-pinyin-search--buffer-has-chinese-p
       (ivy-state-buffer ivy-last)))
     ((and (not (ivy-state-dynamic-collection ivy-last))
           (or (listp collection) (vectorp collection)))
      (ivy-pinyin-search--collection-has-chinese-p collection)))))

(defun ivy-pinyin-search--automatic-input-p (input)
  "Return non-nil when INPUT is suitable for automatic pinyin matching."
  (and ivy-pinyin-search-auto-detect
       (string-match-p "\\`[A-Za-z \\t]+\\'" input)
       (ivy-pinyin-search--source-has-chinese-p)))

(defun ivy-pinyin-search--candidate-string (definition)
  "Return all characters represented by Quail DEFINITION as a string."
  (cond
   ((characterp definition) (char-to-string definition))
   ((stringp definition) definition)
   ((vectorp definition)
    (mapconcat (lambda (candidate)
                 (if (characterp candidate)
                     (char-to-string candidate)
                   candidate))
               definition ""))))

(defun ivy-pinyin-search--load-quail-package ()
  "Load the built-in `chinese-py' Quail package without selecting it."
  (unless (quail-package "chinese-py")
    (let ((saved-default (default-value 'quail-current-package)))
      (unwind-protect
          (with-temp-buffer
            (load "quail/PY" nil t))
        (setq-default quail-current-package saved-default))))
  (or (quail-package "chinese-py")
      (error "The built-in Chinese pinyin input method is unavailable")))

(defun ivy-pinyin-search--ensure-tables ()
  "Build cached full-syllable and initial-letter lookup tables."
  (unless ivy-pinyin-search--syllables
    (let ((package (ivy-pinyin-search--load-quail-package))
          (syllables (make-hash-table :test #'equal))
          (initials (make-hash-table :test #'eql))
          (maximum-length 0))
      (cl-labels
          ((walk
            (node spelling)
            (let ((candidates
                   (ivy-pinyin-search--candidate-string (car-safe node))))
              (when candidates
                (puthash spelling candidates syllables)
                (setq maximum-length (max maximum-length (length spelling)))
                (let ((initial (aref spelling 0)))
                  (puthash initial
                           (concat (gethash initial initials) candidates)
                           initials))))
            (dolist (branch (cdr-safe node))
              (walk (cdr branch)
                    (concat spelling (char-to-string (car branch)))))))
        (walk (nth 2 package) ""))
      (maphash
       (lambda (initial candidates)
         (puthash initial
                  (apply #'string
                         (delete-dups (string-to-list candidates)))
                  initials))
       initials)
      (setq ivy-pinyin-search--syllables syllables
            ivy-pinyin-search--initials initials
            ivy-pinyin-search--maximum-syllable-length maximum-length))))

(defun ivy-pinyin-search--character-class (characters)
  "Return a regexp character class containing CHARACTERS."
  (concat "[" (regexp-quote characters) "]"))

(defun ivy-pinyin-search--segment (word)
  "Split WORD greedily into valid full-pinyin syllables."
  (ivy-pinyin-search--ensure-tables)
  (let ((position 0)
        (length (length word))
        segments
        failed)
    (while (and (< position length) (not failed))
      (let ((end (min length
                      (+ position ivy-pinyin-search--maximum-syllable-length)))
            syllable)
        (while (and (> end position) (not syllable))
          (let ((candidate (substring word position end)))
            (when (gethash candidate ivy-pinyin-search--syllables)
              (setq syllable candidate)))
          (setq end (1- end)))
        (if syllable
            (progn
              (push syllable segments)
              (setq position (+ position (length syllable))))
          (setq failed t))))
    (unless failed
      (nreverse segments))))

(defun ivy-pinyin-search--full-regexp (text)
  "Return a full-pinyin regexp for TEXT, or nil when it cannot be parsed."
  (let ((words (split-string text "[[:space:]]+" t))
        regexps
        (valid t))
    (dolist (word words)
      (let ((segments (ivy-pinyin-search--segment word)))
        (if segments
            (push
             (mapconcat
              (lambda (syllable)
                (ivy-pinyin-search--character-class
                 (gethash syllable ivy-pinyin-search--syllables)))
              segments "")
             regexps)
          (setq valid nil))))
    (when (and valid words)
      (mapconcat #'identity (nreverse regexps) ".*"))))

(defun ivy-pinyin-search--initial-regexp (text &optional chinese-only)
  "Return an initial-letter pinyin regexp for TEXT.
Exclude literal Latin letters when CHINESE-ONLY is non-nil."
  (ivy-pinyin-search--ensure-tables)
  (let (parts invalid has-chinese)
    (dolist (character (string-to-list text))
      (let ((candidates (gethash character ivy-pinyin-search--initials)))
        (cond
         ((memq character '(?\s ?\t))
          (push ".*" parts))
         (candidates
          (setq has-chinese t)
          (push (ivy-pinyin-search--character-class
                 (if chinese-only
                     candidates
                   (concat (char-to-string character) candidates)))
                parts))
         (chinese-only
          (setq invalid t))
         (t
          (push (regexp-quote (char-to-string character)) parts)))))
    (when (and (not invalid) (or (not chinese-only) has-chinese))
      (mapconcat #'identity (nreverse parts) ""))))

(defun ivy-pinyin-search--pinyin-regexp (input)
  "Return a pinyin regexp for Ivy INPUT when forced or auto-detected."
  (let ((forced (and (not (string-empty-p ivy-pinyin-search-suffix))
                     (string-suffix-p ivy-pinyin-search-suffix input))))
    (when (or forced (ivy-pinyin-search--automatic-input-p input))
      (let ((text (downcase
                   (if forced
                       (substring input 0
                                  (- (length input)
                                     (length ivy-pinyin-search-suffix)))
                     input))))
        (if (string-empty-p text)
            (if forced "\\b\\B" "")
          (let* ((initials (ivy-pinyin-search--initial-regexp text forced))
                 (full (ivy-pinyin-search--full-regexp text))
                 (regexps (delete-dups (delq nil (list full initials)))))
            (cond
             ((null regexps) "\\b\\B")
             ((null (cdr regexps)) (car regexps))
             (t (concat "\\(?:" (mapconcat #'identity regexps "\\|")
                        "\\)")))))))))

(defun ivy-pinyin-search--regexp-builder (input)
  "Build a pinyin regexp for INPUT, otherwise use Ivy's prior builder."
  (or (ivy-pinyin-search--pinyin-regexp input)
      (funcall (or ivy-pinyin-search--fallback-builder #'ivy--regex-plus)
               input)))

(define-minor-mode ivy-pinyin-search-mode
  "Globally enable detected or forced initial and full-pinyin Ivy matching."
  :global t
  :group 'ivy-pinyin-search
  (let ((entry (assq t ivy-re-builders-alist)))
    (if ivy-pinyin-search-mode
        (progn
          (setq ivy-pinyin-search--had-default-builder (and entry t)
                ivy-pinyin-search--fallback-builder
                (if entry (cdr entry) #'ivy--regex-plus))
          (if entry
              (setcdr entry #'ivy-pinyin-search--regexp-builder)
            (push '(t . ivy-pinyin-search--regexp-builder)
                  ivy-re-builders-alist)))
      (when (and entry
                 (eq (cdr entry) #'ivy-pinyin-search--regexp-builder))
        (if ivy-pinyin-search--had-default-builder
            (setcdr entry ivy-pinyin-search--fallback-builder)
          (setq ivy-re-builders-alist
                (delq entry ivy-re-builders-alist))))
      (setq ivy-pinyin-search--fallback-builder nil
            ivy-pinyin-search--had-default-builder nil
            ivy-pinyin-search--cached-collection nil
            ivy-pinyin-search--cached-collection-result nil))))

(provide 'ivy-pinyin-search)

;;; ivy-pinyin-search.el ends here
