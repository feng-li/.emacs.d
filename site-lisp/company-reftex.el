;;; company-reftex.el --- Company backend based on RefTeX -*- lexical-binding: t -*-

;; Copyright (C) 2026 Feng Li
;; Copyright (C) 2018 TheBB
;;
;; Author: Feng Li <m@feng.li>
;; URL: https://github.com/feng-li/.emacs.d/blob/master/site-lisp/company-reftex.el
;; Version: 0.2.0
;; Keywords: bib tex company latex reftex references labels citations
;; Package-Requires: ((emacs "25.1") (s "1.12") (company "0.8"))
;;
;; This file is a cache-enabled redesign of company-reftex 0.1.0.
;; It preserves the original public backend and helper APIs.

;; This program is free software; you can redistribute it and/or modify
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

;; Company backends for RefTeX citations and labels, with an in-memory
;; bibliography cache.  The public API is compatible with company-reftex
;; 0.1.0.
;;
;; Usage
;; -----
;;
;; Put this file somewhere in `load-path', then load it:
;;
;;   (require 'company-reftex)
;;
;; For AUCTeX, a minimal setup is:
;;
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (reftex-mode 1)
;;               (make-local-variable 'company-backends)
;;               (add-to-list 'company-backends 'company-reftex-citations)
;;               (add-to-list 'company-backends 'company-reftex-labels)))
;;
;; If `company-mode' is not enabled globally, add:
;;
;;   (add-hook 'LaTeX-mode-hook #'company-mode)
;;
;; For Emacs' built-in `latex-mode', use `latex-mode-hook' instead of
;; `LaTeX-mode-hook'.
;;
;; Citation completion works like the original package.  For example, while
;; point is inside
;;
;;   \cite{hynd
;;
;; Company asks `company-reftex-citations' for candidates.  The first query
;; parses the bibliography through RefTeX and stores the parsed entries in
;; Emacs memory.  Later queries filter that cache instead of reparsing the
;; .bib files on every keystroke.
;;
;; The cache is automatically invalidated when a bibliography file changes
;; on disk or an associated BibTeX buffer has unsaved changes.  To clear it
;; manually:
;;
;;   M-x company-reftex-clear-cache
;;
;; File metadata are checked at most once per second by default.  To change
;; that interval:
;;
;;   (setq company-reftex-cache-validation-interval 5.0)
;;
;; A value of 0 checks for changed bibliography files on every candidate
;; request, while still reusing parsed entries whenever the files are
;; unchanged.
;;
;; In multi-file AUCTeX documents, set `TeX-master' normally; no special
;; cache configuration is required.

;;; Code:

(eval-when-compile
  (require 'rx))

(require 'cl-lib)
(require 'company)
(require 'reftex)
(require 'reftex-cite)
(require 's)


;; Customization

(defgroup company-reftex nil
  "Completion backend for RefTeX."
  :prefix "company-reftex-"
  :tag "Company RefTeX"
  :group 'company)

(defcustom company-reftex-annotate-citations "%t"
  "If non-nil, a format string with which to annotate citations.
See `reftex-format-citation'."
  :type '(choice string (const nil))
  :group 'company-reftex)

(defcustom company-reftex-annotate-labels t
  "Whether to annotate labels with their contents."
  :type 'boolean
  :group 'company-reftex)

(defcustom company-reftex-max-annotation-length nil
  "Truncate annotations to this length."
  :type '(choice (const :tag "Off" nil) integer)
  :group 'company-reftex)

(defcustom company-reftex-cache-validation-interval 1.0
  "Minimum seconds between bibliography cache validation checks.

The citation cache is still used while this interval is running, without
checking bibliography file metadata again.  A value of 0 checks file
metadata for every new candidate request; it does not reparse unchanged
bibliography files."
  :type 'number
  :group 'company-reftex)

(defcustom company-reftex-labels-regexp
  (rx "\\"
      ;; List taken from `reftex-ref-style-alist'.
      (or "autoref"
          "autopageref"
          "Cpageref"
          "cpageref"
          "Cref"
          "cref"
          "eqref"
          "Fref"
          "fref"
          "pageref"
          "Ref"
          "ref"
          "vpageref"
          "Vref"
          "vref")
      "{"
      (group (* (not (any "}"))))
      (regexp "\\="))
  "Regular expression to use when looking for the label prefix.
Group number 1 should be the prefix itself."
  :type 'string
  :group 'company-reftex)

(defcustom company-reftex-citations-regexp
  (rx "\\"
      ;; List taken from `reftex-cite-format-builtin'.
      (or "autocite"
          "autocite*"
          "bibentry"
          "cite"
          "cite*"
          "citeA"
          "citeaffixed"
          "citeasnoun"
          "citeauthor"
          "citeauthor*"
          "citeauthory"
          "citefield"
          "citeN"
          "citename"
          "cites"
          "citet"
          "citet*"
          "citetitle"
          "citetitle*"
          "citep"
          "citeyear"
          "citeyear*"
          "footcite"
          "footfullcite"
          "fullcite"
          "fullocite"
          "nocite"
          "ocite"
          "ocites"
          "parencite"
          "parencite*"
          "possessivecite"
          "shortciteA"
          "shortciteN"
          "smartcite"
          "textcite"
          "textcite*"
          "ycite"
          "ycites")
      (* (not (any "[{")))
      (* (seq "[" (* (not (any "]"))) "]"))
      "{"
      (* (seq (* (not (any "},"))) ","))
      (group (* (not (any "},")))))
  "Regular expression to use when looking for the citation prefix.
Group number 1 should be the prefix itself."
  :type 'string
  :group 'company-reftex)


;; Cache

(cl-defstruct
    (company-reftex--cache
     (:constructor company-reftex--make-cache))
  signature
  entries
  matches
  candidates
  checked-at)

(defvar company-reftex--citation-caches
  (make-hash-table :test #'equal)
  "Bibliography caches keyed by bibliography source set.")

(defconst company-reftex--cache-miss
  (make-symbol "company-reftex-cache-miss"))

(defun company-reftex--source-file (source)
  "Return the file name represented by SOURCE, or nil."
  (cond
   ((bufferp source)
    (and (buffer-live-p source) (buffer-file-name source)))
   ((stringp source)
    (expand-file-name (substring-no-properties source)))
   (t nil)))

(defun company-reftex--source-buffer (source)
  "Return a live buffer corresponding to SOURCE, or nil."
  (cond
   ((bufferp source)
    (and (buffer-live-p source) source))
   ((stringp source)
    (let ((file (company-reftex--source-file source)))
      (or (and file (find-buffer-visiting file))
          (get-buffer source))))
   (t nil)))

(defun company-reftex--source-id (source)
  "Return a stable cache identity for SOURCE."
  (let ((file (company-reftex--source-file source)))
    (cond
     (file file)
     ((bufferp source) (list :buffer source))
     (t source))))

(defun company-reftex--source-state (source)
  "Return a lightweight modification signature for SOURCE."
  (let* ((buffer (company-reftex--source-buffer source))
         (file (or (and buffer (buffer-file-name buffer))
                   (company-reftex--source-file source)))
         (attributes
          (and file
               (condition-case nil
                   (file-attributes file)
                 (file-error nil))))
         (buffer-tick
          (and buffer
               (or (null file) (buffer-modified-p buffer))
               (buffer-chars-modified-tick buffer))))
    (list (company-reftex--source-id source)
          buffer-tick
          (and attributes (nth 5 attributes))
          (and attributes (nth 7 attributes)))))

(defun company-reftex--source-signature (sources)
  "Return the combined modification signature of SOURCES."
  (mapcar #'company-reftex--source-state sources))

(defun company-reftex--citation-source ()
  "Return citation source as (TYPE . SOURCES), or nil.
TYPE is either `bib' or `thebib'."
  (let ((bibtype (reftex-bib-or-thebib)))
    (cond
     ((eq bibtype 'thebib)
      (cons
       'thebib
       (reftex-uniquify
        (mapcar #'cdr
                (reftex-all-assq
                 'thebib (symbol-value reftex-docstruct-symbol))))))
     ((eq bibtype 'bib)
      (cons 'bib (reftex-get-bibfile-list)))
     (reftex-default-bibliography
      (let ((files (reftex-default-bibliography)))
        (and files (cons 'bib files)))))))

(defun company-reftex--cache-key (type sources)
  "Return the cache key for TYPE and SOURCES."
  (list type
        (mapcar #'company-reftex--source-id sources)
        reftex-sort-bibtex-matches))

(defun company-reftex--extract-all-citation-entries (type sources)
  "Extract all citation entries of TYPE from SOURCES once."
  (let ((search-regexp
         (if (eq type 'bib)
             "@\\(?:\\w\\|\\s_\\)+[ \t\n\r]*[{(]"
           ".+")))
    ;; RefTeX normally prompts for search regexps.  Supply a regexp that
    ;; finds every entry so parsing happens once, outside Company queries.
    (cl-letf (((symbol-function 'reftex--query-search-regexps)
               (lambda (_) (list search-regexp))))
      (cond
       ((eq type 'thebib)
        (reftex-extract-bib-entries-from-thebibliography sources))
       ((eq type 'bib)
        (reftex-extract-bib-entries sources))))))

(defun company-reftex--build-cache (type sources)
  "Build and return a citation cache for TYPE and SOURCES."
  (let* ((entries
          (company-reftex--extract-all-citation-entries type sources))
         (matches (make-hash-table :test #'equal))
         (candidates (make-hash-table :test #'eq)))
    ;; Every query is a refinement of the empty query.  Keeping this entry
    ;; also lets an empty prefix return immediately.
    (puthash "" entries matches)
    (company-reftex--make-cache
     :signature (company-reftex--source-signature sources)
     :entries entries
     :matches matches
     :candidates candidates
     :checked-at (float-time))))

(defun company-reftex--get-cache (type sources)
  "Return a current citation cache for TYPE and SOURCES."
  (let* ((key (company-reftex--cache-key type sources))
         (cache (gethash key company-reftex--citation-caches))
         (now (float-time))
         (interval (max 0 company-reftex-cache-validation-interval))
         (recent
          (and cache
               (< (- now (company-reftex--cache-checked-at cache))
                  interval))))
    (cond
     (recent cache)
     ((and cache
           (equal (company-reftex--cache-signature cache)
                  (company-reftex--source-signature sources)))
      (setf (company-reftex--cache-checked-at cache) now)
      cache)
     (t
      (setq cache (company-reftex--build-cache type sources))
      (puthash key cache company-reftex--citation-caches)
      cache))))

(defun company-reftex--matching-entries (cache prefix)
  "Return cached entries from CACHE whose raw text matches PREFIX."
  (let* ((table (company-reftex--cache-matches cache))
         (cached (gethash prefix table company-reftex--cache-miss)))
    (if (not (eq cached company-reftex--cache-miss))
        cached
      ;; Reuse the longest already cached prefix.  While a user types a
      ;; citation, each query normally refines the preceding result set.
      (let ((length (length prefix))
            (base company-reftex--cache-miss))
        (while (and (eq base company-reftex--cache-miss)
                    (> length 0))
          (setq length (1- length)
                base (gethash (substring prefix 0 length)
                              table company-reftex--cache-miss)))
        (when (eq base company-reftex--cache-miss)
          (setq base (company-reftex--cache-entries cache)))
        (let ((regexp (regexp-quote prefix))
              (case-fold-search t))
          (setq cached
                (cl-loop
                 for entry in base
                 for raw = (cdr (assoc "&entry" entry))
                 when (and raw (string-match-p regexp raw))
                 collect entry)))
        (puthash prefix cached table)
        cached))))

(defun company-reftex--annotation-state ()
  "Return settings that affect cached citation annotations."
  (list company-reftex-annotate-citations
        company-reftex-max-annotation-length
        (and (boundp 'reftex-cite-punctuation)
             (copy-tree reftex-cite-punctuation))
        (and (boundp 'reftex-comment-citations)
             reftex-comment-citations)
        (and (boundp 'reftex-cite-comment-format)
             reftex-cite-comment-format)
        (and (boundp 'reftex-abbrev-parameters)
             (copy-tree reftex-abbrev-parameters))))

(defun company-reftex--cached-candidate (cache entry state)
  "Return the cached Company candidate for ENTRY under STATE."
  (let* ((table (company-reftex--cache-candidates cache))
         (cached (gethash entry table company-reftex--cache-miss)))
    (if (and (not (eq cached company-reftex--cache-miss))
             (equal (car cached) state))
        (cdr cached)
      (let* ((citation-key (substring-no-properties (car entry)))
             (candidate
              (company-reftex-annotate
               citation-key
               (when company-reftex-annotate-citations
                 (reftex-format-citation
                  entry company-reftex-annotate-citations)))))
        (puthash entry (cons state candidate) table)
        candidate))))

(defun company-reftex--cached-citation-candidates (cache prefix)
  "Return citation candidates from CACHE matching PREFIX."
  (let ((state (company-reftex--annotation-state)))
    (cl-loop
     for entry in (company-reftex--matching-entries cache prefix)
     ;; Company may attach its own properties to candidate strings.
     collect (copy-sequence
              (company-reftex--cached-candidate cache entry state)))))

;;;###autoload
(defun company-reftex-clear-cache ()
  "Clear all in-memory citation caches used by company-reftex."
  (interactive)
  (clrhash company-reftex--citation-caches)
  ;; Also discard Company's current per-session candidate cache when the
  ;; command is called from an active completion buffer.
  (when (boundp 'company-candidates-cache)
    (setq company-candidates-cache nil))
  (when (called-interactively-p 'interactive)
    (message "company-reftex citation cache cleared")))


;; Auxiliary functions

(defun company-reftex-prefix (regexp)
  "Return the prefix for matching given REGEXP."
  (and (derived-mode-p 'latex-mode)
       reftex-mode
       (when (looking-back regexp nil)
         (match-string-no-properties 1))))

(defun company-reftex-annotate (key annotation)
  "Annotate KEY with ANNOTATION if the latter is not nil.
Obey the setting of `company-reftex-max-annotation-length'."
  (cond
   ((not annotation) key)
   ((not company-reftex-max-annotation-length)
    (propertize key 'reftex-annotation annotation))
   (t
    (propertize
     key 'reftex-annotation
     (s-truncate company-reftex-max-annotation-length annotation)))))


;; Citations

(defun company-reftex-citation-candidates (prefix)
  "Find all citation candidates matching PREFIX.

Bibliography entries are parsed once and retained in an invalidation-aware
in-memory cache.  Matching preserves the original company-reftex behavior:
PREFIX is searched literally and case-insensitively across each raw BibTeX
entry, not only its citation key."
  (reftex-access-scan-info)
  (let ((source (company-reftex--citation-source)))
    (when source
      (let* ((type (car source))
             (sources (cdr source))
             (cache (company-reftex--get-cache type sources)))
        (company-reftex--cached-citation-candidates cache prefix)))))

;;;###autoload
(defun company-reftex-citations (command &optional arg &rest _)
  "Company backend for LaTeX citations, powered by RefTeX.
For more information on COMMAND and ARG, see `company-backends'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-reftex-citations))
    (prefix (company-reftex-prefix company-reftex-citations-regexp))
    (candidates (company-reftex-citation-candidates arg))
    (annotation
     (when company-reftex-annotate-citations
       (concat
        (unless company-tooltip-align-annotations " -> ")
        (get-text-property 0 'reftex-annotation arg))))))


;; Labels

(defun company-reftex-label-candidates (prefix)
  "Find all label candidates matching PREFIX."
  (reftex-access-scan-info)
  (reftex-parse-all)
  (cl-loop
   for entry in (symbol-value reftex-docstruct-symbol)
   if (and (stringp (car entry))
           (string-prefix-p prefix (car entry)))
   collect
   (company-reftex-annotate (car entry) (cl-caddr entry))))

;;;###autoload
(defun company-reftex-labels (command &optional arg &rest _)
  "Company backend for LaTeX labels, powered by RefTeX.
For more information on COMMAND and ARG, see `company-backends'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-reftex-labels))
    (prefix (company-reftex-prefix company-reftex-labels-regexp))
    (candidates (company-reftex-label-candidates arg))
    (annotation
     (when company-reftex-annotate-labels
       (concat
        (unless company-tooltip-align-annotations " -> ")
        (get-text-property 0 'reftex-annotation arg))))))

(provide 'company-reftex)

;;; company-reftex.el ends here
