;;; my-synosaurus-mdx.el --- Local MDX backend for Synosaurus -*- lexical-binding: t; -*-

;;; Commentary:
;; Query a Merriam-Webster MDict dictionary for direct synonyms and fall back
;; to Soule's StarDict thesaurus when the MDX entry has no synonym section.

;;; Code:

(require 'dom)
(require 'json)
(require 'seq)
(require 'subr-x)

(defvar my-synosaurus-mdict-program nil
  "Program used to query local MDict dictionaries.")

(defvar my-synosaurus-mdx-file nil
  "Merriam-Webster MDX file used by Synosaurus.")

(defvar my-synosaurus-soule-dictionary
  "Soule's Dictionary of English Synonyms (En-En)"
  "StarDict thesaurus used when the MDX file has no synonyms.")

(defvar my-synosaurus--mdx-key-index nil
  "Case-insensitive lookup keys from `my-synosaurus-mdx-file'.")

(defvar my-synosaurus--mdx-stripped-key-index nil
  "Punctuation-insensitive lookup keys from `my-synosaurus-mdx-file'.")

(defun my-synosaurus--normalize-mdx-key (key)
  "Normalize MDX lookup KEY according to its StripKey setting."
  (downcase (replace-regexp-in-string "[^[:alnum:]]" "" key)))

(defun my-synosaurus--ensure-mdx-key-index ()
  "Build and cache a case- and punctuation-insensitive MDX key index."
  (unless (and my-synosaurus--mdx-key-index
               my-synosaurus--mdx-stripped-key-index)
    (unless (and my-synosaurus-mdict-program
                 (file-executable-p my-synosaurus-mdict-program))
      (error "mdict is not executable: %s" my-synosaurus-mdict-program))
    (unless (and my-synosaurus-mdx-file
                 (file-readable-p my-synosaurus-mdx-file))
      (error "MDX dictionary is not readable: %s" my-synosaurus-mdx-file))
    (let ((index (make-hash-table :test #'equal))
          (stripped-index (make-hash-table :test #'equal)))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix)
              (status
               (call-process my-synosaurus-mdict-program nil t nil
                             "-k" my-synosaurus-mdx-file)))
          (unless (zerop status)
            (error "mdict key listing failed with status %s" status)))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((key (string-trim
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position)))))
            (unless (string-empty-p key)
              (puthash (downcase key) key index)
              (let ((stripped (my-synosaurus--normalize-mdx-key key)))
                (unless (gethash stripped stripped-index)
                  (puthash stripped key stripped-index)))))
          (forward-line 1)))
      (setq my-synosaurus--mdx-key-index index
            my-synosaurus--mdx-stripped-key-index stripped-index)))
  my-synosaurus--mdx-key-index)

(defun my-synosaurus--resolve-mdx-key (word)
  "Resolve WORD to its exact MDX key, ignoring case before punctuation."
  (my-synosaurus--ensure-mdx-key-index)
  (or (gethash (downcase word) my-synosaurus--mdx-key-index)
      (gethash (my-synosaurus--normalize-mdx-key word)
               my-synosaurus--mdx-stripped-key-index)))

(defun my-synosaurus--mdx-entry (word &optional seen)
  "Return the MDX HTML entry for WORD, following safe redirects.
SEEN contains lower-case keys already followed."
  (let* ((key (my-synosaurus--resolve-mdx-key word))
         (seen-key (and key (downcase key))))
    (when (and key (not (member seen-key seen)))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix)
              (status
               (call-process my-synosaurus-mdict-program nil t nil
                             "-q" key my-synosaurus-mdx-file)))
          (unless (zerop status)
            (error "mdict lookup failed with status %s" status)))
        (let ((entry (buffer-string)))
          (if (string-match "\\`@@@LINK=\\([^\r\n\0]+\\)" entry)
              (let ((target (match-string 1 entry)))
                ;; Follow spelling/punctuation aliases, but not derivational
                ;; redirects that would change part of speech or inflection.
                (when (string-equal
                       (my-synosaurus--normalize-mdx-key word)
                       (my-synosaurus--normalize-mdx-key target))
                  (my-synosaurus--mdx-entry
                   target (cons seen-key seen))))
            (and (string-match-p "<[^>]+>" entry) entry)))))))

(defun my-synosaurus--clean-synonym (synonym)
  "Remove dictionary annotations from SYNONYM."
  (let ((clean (string-trim synonym)))
    (setq clean (replace-regexp-in-string "\\`[|*]+" "" clean))
    (setq clean (replace-regexp-in-string
                 "[[:space:]]+[0-9]+$" "" clean))
    (setq clean (replace-regexp-in-string "[[:space:]]+" " " clean))
    (setq clean (string-trim-right clean "[[:space:].]+"))
    (unless (string-empty-p clean)
      clean)))

(defun my-synosaurus--candidate-group (text word)
  "Parse synonym list TEXT and remove WORD from the candidates."
  (setq text (replace-regexp-in-string
              "\\`[[:space:]]*synonyms:?[[:space:]]*" "" text t))
  (delete-dups
   (delq nil
         (mapcar
          (lambda (candidate)
            (let ((clean (my-synosaurus--clean-synonym candidate)))
              (unless (and clean (string-equal-ignore-case clean word))
                clean)))
          (split-string text "[,;]" t)))))

(defun my-synosaurus--mdx-groups (word)
  "Return direct synonym groups for WORD from the Merriam-Webster MDX."
  (when-let* ((entry (my-synosaurus--mdx-entry word)))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (when (search-forward "\0" nil t)
        (delete-region (1- (point)) (point-max)))
      (let* ((tree (libxml-parse-html-region (point-min) (point-max)))
             (labels (dom-by-class tree "tec"))
             groups)
        ;; Prefer the explicitly separated thesaurus senses.
        (dolist (label labels)
          (when-let* ((container
                       (seq-find
                        (lambda (node) (memq label (dom-children node)))
                        (dom-by-tag tree 'div)))
                      (group
                       (my-synosaurus--candidate-group
                        (dom-texts container) word)))
            (when group (push group groups))))
        ;; Some dictionary-only entries have a shorter Synonyms field.
        (unless groups
          (dolist (dl (dom-by-tag tree 'dl))
            (when (dom-by-class dl "ltp")
              (when-let* ((dd (car (dom-by-tag dl 'dd)))
                          (group
                           (my-synosaurus--candidate-group
                            (dom-texts dd) word)))
                (when group (push group groups))))))
        (nreverse groups)))))

(defun my-synosaurus--soule-heading-p (line)
  "Return non-nil when LINE is a Soule part-of-speech or sense heading."
  (or (string-match-p "\\`[0-9]+\\.\\'" line)
      (string-match-p
       "\\`[IVXLCDM]+\\.\\(?:[[:space:]]+[anv]\\.\\(?:[[:space:]]+[anv]\\.\\)?\\)?\\'"
       line)
      (string-match-p
       "\\`[anv]\\.\\(?:[[:space:]]+[anv]\\.\\)?\\'" line)))

(defun my-synosaurus--soule-groups (word)
  "Return synonym groups for WORD from the local Soule StarDict data."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)
          (status
           (call-process
            "sdcv" nil t nil
            "--data-dir"
            (expand-file-name "dict/sdcv" user-emacs-directory)
            "--only-data-dir"
            "--use-dict" my-synosaurus-soule-dictionary
            "--non-interactive"
            "--exact-search"
            "--json-output"
            word)))
      (unless (zerop status)
        (error "sdcv failed with status %s" status))
      (let ((entries
             (json-parse-string
              (buffer-string)
              :object-type 'alist
              :array-type 'list))
            groups)
        (dolist (entry entries (nreverse groups))
          (dolist (line (split-string
                         (alist-get 'definition entry) "\n" t "[[:space:]]+"))
            (unless (my-synosaurus--soule-heading-p line)
              (let ((group (my-synosaurus--candidate-group line word)))
                (when group (push group groups))))))))))

(defun my-synosaurus-backend-local-thesauri (word)
  "Return synonyms for WORD from Merriam-Webster MDX, then Soule."
  (let ((mdx-groups (my-synosaurus--mdx-groups word)))
    (if mdx-groups
        (progn
          (message "Synosaurus: using Merriam-Webster Collegiate Thesaurus (2015)")
          mdx-groups)
      (let ((soule-groups (my-synosaurus--soule-groups word)))
        (if soule-groups
            (message "Synosaurus: using Soule's Dictionary (Merriam-Webster had no synonyms)")
          (message "Synosaurus: no synonyms in Merriam-Webster or Soule's Dictionary"))
        soule-groups))))

(provide 'my-synosaurus-mdx)

;;; my-synosaurus-mdx.el ends here
