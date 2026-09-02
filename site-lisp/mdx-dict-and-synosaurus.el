;;; mdx-dict-and-synosaurus.el --- Local dictionaries and thesaurus -*- lexical-binding: t; -*-

;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (company "0.10.0"))
;; Author: Feng Li <m@feng.li>
;; Keywords: convenience, dictionary, thesaurus, text
;; Copyright (C) 2026 Feng Li <m@feng.li>
;; Copyright (C) 2019 Hans-Peter Deifel
;;
;; The Synosaurus frontend in this file is based on the original work by
;; Hans-Peter Deifel <hpd@hpdeifel.de>.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This standalone package provides two related interfaces:
;;
;; - `mdx-dict-search' browses local MDict (MDX) dictionaries through the
;;   mdict command and can include selected StarDict dictionaries via sdcv.
;; - `synosaurus-mode' provides lookup, insertion, and replacement commands
;;   backed by a configured Merriam-Webster MDX thesaurus, with Soule's
;;   StarDict thesaurus as a fallback.
;;
;; MDX headwords and queried entries are cached for the Emacs session.  Use
;; `mdx-dict-clear-cache' after replacing a dictionary file.

;;; History:
;; 0.1.0 - Combine MDX/StarDict browsing with the Synosaurus frontend.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'company)
(require 'dom)
(require 'ido)
(require 'json)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'thingatpt)
(require 'url-util)

(declare-function popup-menu* "popup")

(defgroup mdx-dict nil
  "Browse local dictionaries and use them as a thesaurus."
  :group 'applications
  :group 'text)

(defcustom mdx-dict-program "mdict"
  "Program used to query local MDict dictionaries."
  :type 'file
  :group 'mdx-dict)

(defcustom mdx-dict-dictionaries nil
  "Alist mapping display names to local MDX files."
  :type '(alist :key-type string :value-type file)
  :group 'mdx-dict)

(defcustom mdx-dict-sdcv-program "sdcv"
  "Program used to query local StarDict dictionaries."
  :type 'file
  :group 'mdx-dict)

(defcustom mdx-dict-sdcv-directory nil
  "Directory containing local StarDict dictionaries."
  :type '(choice (const nil) directory)
  :group 'mdx-dict)

(defcustom mdx-dict-sdcv-dictionaries nil
  "StarDict dictionary names included in `mdx-dict-search'."
  :type '(repeat string)
  :group 'mdx-dict)

(defcustom mdx-dict-synosaurus-mdx-file nil
  "Merriam-Webster MDX file used by Synosaurus."
  :type '(choice (const nil) file)
  :group 'mdx-dict)

(defcustom mdx-dict-synosaurus-soule-dictionary
  "Soule's Dictionary of English Synonyms (En-En)"
  "StarDict thesaurus used when the MDX file has no synonyms."
  :type 'string
  :group 'mdx-dict)

(defgroup synosaurus nil
  "Look up, insert, and replace words using a thesaurus backend."
  :group 'mdx-dict
  :group 'convenience)

(defcustom synosaurus-choose-method 'company
  "Completion interface used to choose a replacement.

The value `company' uses a Company popup in the current buffer, `popup' uses
popup.el, `ido' uses IDO, and `default' uses `completing-read'."
  :type '(choice (const :tag "Company" company)
                 (const :tag "popup.el" popup)
                 (const :tag "IDO" ido)
                 (const :tag "Standard completion" default))
  :group 'synosaurus)

(defcustom synosaurus-backend #'mdx-dict-synosaurus-backend
  "Function used to return synonym groups for a word."
  :type 'function
  :group 'synosaurus)
(make-variable-buffer-local 'synosaurus-backend)

(defun synosaurus--set-prefix (symbol value)
  "Set key prefix SYMBOL to VALUE and update `synosaurus-mode-map'."
  (when (and (boundp 'synosaurus-mode-map)
             (keymapp synosaurus-mode-map)
             (boundp symbol))
    (define-key synosaurus-mode-map (default-value symbol) nil)
    (define-key synosaurus-mode-map value 'synosaurus-command-map))
  (set-default symbol value))

(defcustom synosaurus-prefix (kbd "C-c C-s")
  "Prefix key for `synosaurus-command-map'."
  :type 'key-sequence
  :set #'synosaurus--set-prefix
  :group 'synosaurus)

(defvar synosaurus--history nil
  "Minibuffer history for Synosaurus lookups.")

(defvar-local synosaurus--company-start-marker nil)
(defvar-local synosaurus--company-end-marker nil)
(defvar-local synosaurus--company-candidates nil)

(defun synosaurus--company-cleanup (&rest _)
  "Clear state left by a Synosaurus Company selection."
  (when (markerp synosaurus--company-start-marker)
    (set-marker synosaurus--company-start-marker nil))
  (when (markerp synosaurus--company-end-marker)
    (set-marker synosaurus--company-end-marker nil))
  (setq synosaurus--company-start-marker nil
        synosaurus--company-end-marker nil
        synosaurus--company-candidates nil)
  (remove-hook 'company-after-completion-hook
               #'synosaurus--company-cleanup t)
  (remove-hook 'company-completion-cancelled-hook
               #'synosaurus--company-cleanup t))

(defun synosaurus--company (command &optional _arg &rest rest)
  "Company backend for choosing Synosaurus replacements.

COMMAND and REST follow the Company backend protocol."
  (pcase command
    ('prefix
     (when (and (markerp synosaurus--company-start-marker)
                (markerp synosaurus--company-end-marker)
                (eq (marker-buffer synosaurus--company-start-marker)
                    (current-buffer))
                (eq (marker-buffer synosaurus--company-end-marker)
                    (current-buffer))
                (<= (marker-position synosaurus--company-start-marker) (point))
                (<= (point) (marker-position synosaurus--company-end-marker)))
       (let ((start (marker-position synosaurus--company-start-marker))
             (end (marker-position synosaurus--company-end-marker)))
         (list (buffer-substring-no-properties start (point))
               (buffer-substring-no-properties (point) end)
               t))))
    ('candidates synosaurus--company-candidates)
    ('adjust-boundaries (cons (car rest) (cadr rest)))
    ('match '((0 . 0)))
    ('kind 'text)
    ('sorted t)
    ('duplicates t)
    ('no-cache t)
    ('require-match t)))

(defun synosaurus--company-start (candidates start end)
  "Choose from CANDIDATES with Company, replacing START through END."
  (when company-candidates
    (company-abort))
  (synosaurus--company-cleanup)
  (setq synosaurus--company-start-marker (copy-marker start)
        synosaurus--company-end-marker (copy-marker end t)
        synosaurus--company-candidates candidates)
  (unless company-mode
    (company-mode 1))
  (add-hook 'company-after-completion-hook
            #'synosaurus--company-cleanup nil t)
  (add-hook 'company-completion-cancelled-hook
            #'synosaurus--company-cleanup nil t)
  (condition-case err
      (company-begin-backend #'synosaurus--company)
    (error
     (synosaurus--company-cleanup)
     (signal (car err) (cdr err)))))

(defun synosaurus--internal-lookup (word)
  "Call `synosaurus-backend' with WORD."
  (unless (functionp synosaurus-backend)
    (user-error "No thesaurus backend is configured"))
  (funcall synosaurus-backend word))

(defun synosaurus--guess-default (&optional errorp)
  "Return the active region or word at point.
When ERRORP is non-nil, signal a user error if neither exists."
  (let ((word
         (if (use-region-p)
             (buffer-substring-no-properties
              (region-beginning) (region-end))
           (thing-at-point 'word t))))
    (when (and errorp (null word))
      (user-error "No word at point"))
    word))

(defun synosaurus--interactive ()
  "Read a word for an interactive Synosaurus command."
  (let ((default (synosaurus--guess-default)))
    (list
     (read-string (if default
                      (format "Word (default %s): " default)
                    "Word: ")
                  nil 'synosaurus--history default))))

(defun synosaurus--button-action (button)
  "Look up the synonym represented by BUTTON."
  (synosaurus-lookup (button-label button)))

(defvar synosaurus-list-mode-map
  (let ((map (copy-keymap button-buffer-map)))
    (set-keymap-parent map special-mode-map)
    map)
  "Keymap for `synosaurus-list-mode'.")

(define-derived-mode synosaurus-list-mode special-mode "Synosaurus"
  "Major mode for browsing a list of synonyms.")

;;;###autoload
(defun synosaurus-lookup (word)
  "Look up WORD with `synosaurus-backend'.
Display the result as clickable words in `*Synonyms List*'."
  (interactive (synosaurus--interactive))
  (let ((synonyms (synosaurus--internal-lookup word))
        (backend synosaurus-backend)
        (buffer (get-buffer-create "*Synonyms List*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'synosaurus-list-mode)
        (synosaurus-list-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Synonyms of %s:\n\n" word)
                            'face 'success))
        (if (null synonyms)
            (insert "No synonyms found.\n")
          (dolist (group synonyms)
            (dolist (synonym (if (listp group) group (list group)))
              (unless (string-equal-ignore-case word synonym)
                (insert " ")
                (insert-text-button synonym
                                    'action #'synosaurus--button-action
                                    'follow-link t)
                (insert "\n")))
            (insert "\n")))
        (goto-char (point-min))
        (condition-case nil
            (forward-button 1 t nil)
          (error nil)))
      (setq-local synosaurus-backend backend))
    (display-buffer buffer)))

(defun synosaurus--flatten-groups (groups word)
  "Flatten synonym GROUPS and remove duplicates and WORD."
  (delete-dups
   (seq-remove
    (lambda (candidate)
      (or (not (stringp candidate))
          (string-empty-p candidate)
          (string-equal-ignore-case candidate word)))
    (cl-loop for group in groups
             if (listp group) append group
             else append (list group)))))

(defun synosaurus--choose (candidates)
  "Choose one item from CANDIDATES."
  (let ((prompt "Replacement: "))
    (pcase synosaurus-choose-method
      ('popup
       (unless (require 'popup nil t)
         (user-error "Install popup.el or change synosaurus-choose-method"))
       (popup-menu* candidates))
      ('ido (ido-completing-read prompt candidates))
      (_ (completing-read prompt candidates nil t)))))

;;;###autoload
(defun synosaurus-choose-and-replace ()
  "Replace the active region or word at point with a chosen synonym."
  (interactive)
  (let* ((regionp (use-region-p))
         (bounds (if regionp
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (word (synosaurus--guess-default t))
         (candidates
          (synosaurus--flatten-groups
           (synosaurus--internal-lookup word) word)))
    (if (null candidates)
        (message "No synonyms found for %s" word)
      (if (eq synosaurus-choose-method 'company)
          (synosaurus--company-start candidates (car bounds) (cdr bounds))
        (when-let* ((replacement (synosaurus--choose candidates)))
          (delete-region (car bounds) (cdr bounds))
          (goto-char (car bounds))
          (insert replacement))))))

;;;###autoload
(defun synosaurus-choose-and-insert (word)
  "Look up WORD and insert a synonym selected by the user."
  (interactive (synosaurus--interactive))
  (let ((candidates
         (synosaurus--flatten-groups
          (synosaurus--internal-lookup word) word)))
    (if (null candidates)
        (message "No synonyms found for %s" word)
      (if (eq synosaurus-choose-method 'company)
          (synosaurus--company-start candidates (point) (point))
        (when-let* ((replacement (synosaurus--choose candidates)))
          (insert replacement))))))

(defvar synosaurus-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'synosaurus-lookup)
    (define-key map (kbd "r") #'synosaurus-choose-and-replace)
    (define-key map (kbd "i") #'synosaurus-choose-and-insert)
    map)
  "Prefix command map for Synosaurus commands.")
(fset 'synosaurus-command-map synosaurus-command-map)

(defvar synosaurus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map synosaurus-prefix 'synosaurus-command-map)
    map)
  "Keymap for `synosaurus-mode'.")

;;;###autoload
(define-minor-mode synosaurus-mode
  "Toggle Synosaurus mode.
This minor mode makes the commands in `synosaurus-command-map' available
under `synosaurus-prefix'."
  :lighter " Syn"
  :keymap synosaurus-mode-map
  :group 'synosaurus)

(defvar mdx-dict--key-cache (make-hash-table :test #'equal)
  "MDX key indexes, keyed by expanded dictionary file name.")

(defvar mdx-dict--entry-cache (make-hash-table :test #'equal)
  "Raw MDX entries, keyed by dictionary file and exact headword.")

(defvar mdx-dict--completion-cache nil
  "Combined completion candidates from `mdx-dict-dictionaries'.")

(defvar mdx-dict--completion-files nil
  "MDX file list used to build `mdx-dict--completion-cache'.")

(defcustom mdx-dict-result-buffer "*MDict*"
  "Buffer used to display local dictionary results."
  :type 'string
  :group 'mdx-dict)

(defvar-local mdx-dict--current-word nil)
(defvar-local mdx-dict--current-sources nil)
(defvar-local mdx-dict--back-history nil)
(defvar-local mdx-dict--forward-history nil)

(defun mdx-dict--normalize-key (key)
  "Normalize MDX lookup KEY according to the common StripKey setting."
  (downcase (replace-regexp-in-string "[^[:alnum:]]" "" key)))

(defun mdx-dict--check-program ()
  "Signal an error unless `mdx-dict-program' is executable."
  (unless (and mdx-dict-program (executable-find mdx-dict-program))
    (error "MDict is not executable: %s" mdx-dict-program)))

(defun mdx-dict--key-data (file)
  "Return cached exact, stripped, and completion keys for MDX FILE."
  (mdx-dict--check-program)
  (setq file (expand-file-name file))
  (unless (file-readable-p file)
    (error "MDX dictionary is not readable: %s" file))
  (or (gethash file mdx-dict--key-cache)
      (let ((exact (make-hash-table :test #'equal))
            (stripped (make-hash-table :test #'equal))
            keys)
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (status (call-process mdx-dict-program nil t nil "-k" file)))
            (unless (eq status 0)
              (error "MDict key listing failed with status %s" status)))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((key (string-trim
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
              (unless (string-empty-p key)
                (push key keys)
                (puthash (downcase key) key exact)
                (let ((normalized (mdx-dict--normalize-key key)))
                  (unless (gethash normalized stripped)
                    (puthash normalized key stripped)))))
            (forward-line 1)))
        (let ((data (list :exact exact
                          :stripped stripped
                          :keys (nreverse keys))))
          (puthash file data mdx-dict--key-cache)
          data))))

(defun mdx-dict--resolve-key (word file)
  "Resolve WORD to an exact headword in MDX FILE."
  (let ((data (mdx-dict--key-data file)))
    (or (gethash (downcase word) (plist-get data :exact))
        (gethash (mdx-dict--normalize-key word)
                 (plist-get data :stripped)))))

(defun mdx-dict--available-dictionaries ()
  "Return configured MDX dictionaries whose files are readable."
  (seq-filter (lambda (dictionary)
                (file-readable-p (cdr dictionary)))
              mdx-dict-dictionaries))

(defun mdx-dict--completion-candidates ()
  "Return cached completion candidates from all configured MDX files."
  (let* ((dictionaries (mdx-dict--available-dictionaries))
         (files (mapcar #'cdr dictionaries)))
    (if (and mdx-dict--completion-cache
             (equal files mdx-dict--completion-files))
        mdx-dict--completion-cache
      (let ((seen (make-hash-table :test #'equal)) candidates)
        (dolist (dictionary dictionaries)
          (dolist (key (plist-get (mdx-dict--key-data (cdr dictionary)) :keys))
            (let ((folded (downcase key)))
              (unless (gethash folded seen)
                (puthash folded t seen)
                (push key candidates)))))
        (setq mdx-dict--completion-files files
              mdx-dict--completion-cache (nreverse candidates))))))

;;;###autoload
(defun mdx-dict-clear-cache ()
  "Clear cached MDX keys, entries, and completion candidates."
  (interactive)
  (setq mdx-dict--key-cache (make-hash-table :test #'equal)
        mdx-dict--entry-cache (make-hash-table :test #'equal)
        mdx-dict--completion-cache nil
        mdx-dict--completion-files nil)
  (message "MDict caches cleared"))

(defun mdx-dict--raw-entry (key file)
  "Return the raw entry for exact KEY in MDX FILE."
  (mdx-dict--check-program)
  (setq file (expand-file-name file))
  (let ((cache-key (cons file key)))
    (or (gethash cache-key mdx-dict--entry-cache)
        (with-temp-buffer
          (let ((coding-system-for-read 'utf-8-unix)
                (status
                 (call-process mdx-dict-program nil t nil "-q" key file)))
            (unless (eq status 0)
              (error "MDict lookup failed with status %s" status)))
          (let ((entry (buffer-string)))
            (when (string-match "\0" entry)
              (setq entry (substring entry 0 (match-beginning 0))))
            (setq entry (string-trim entry))
            (unless (string-empty-p entry)
              (puthash cache-key entry mdx-dict--entry-cache)
              entry))))))

(defun mdx-dict--lookup-entry (word file &optional safe-redirects seen)
  "Return a plist describing WORD's entry in MDX FILE.
When SAFE-REDIRECTS is non-nil, follow only case and punctuation aliases.
SEEN contains lower-case keys already followed."
  (let* ((key (mdx-dict--resolve-key word file))
         (seen-key (and key (downcase key))))
    (when (and key (not (member seen-key seen)))
      (let ((entry (mdx-dict--raw-entry key file)))
        (if (and entry (string-match "\\`@@@LINK=\\([^\r\n]+\\)" entry))
            (let ((target (string-trim (match-string 1 entry))))
              (when (or (not safe-redirects)
                        (string-equal (mdx-dict--normalize-key word)
                                      (mdx-dict--normalize-key target)))
                (mdx-dict--lookup-entry
                 target file safe-redirects (cons seen-key seen))))
          (when (and entry (string-match-p "<[^>]+>" entry))
            (list :requested word :key key :html entry)))))))

(defun mdx-dict-synosaurus--mdx-entry (word)
  "Return Merriam-Webster's HTML entry for WORD using safe redirects."
  (when-let* ((result
               (mdx-dict--lookup-entry word mdx-dict-synosaurus-mdx-file t)))
    (plist-get result :html)))

(defun mdx-dict--sdcv-query (word dictionary)
  "Return sdcv JSON entries for WORD from StarDict DICTIONARY."
  (unless (and mdx-dict-sdcv-program
               (executable-find mdx-dict-sdcv-program))
    (error "SDCV is not executable: %s" mdx-dict-sdcv-program))
  (unless (and mdx-dict-sdcv-directory
               (file-directory-p mdx-dict-sdcv-directory))
    (error "StarDict directory is not readable: %s"
           mdx-dict-sdcv-directory))
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix)
          (status
           (call-process
            mdx-dict-sdcv-program nil t nil
            "--data-dir" mdx-dict-sdcv-directory
            "--only-data-dir"
            "--use-dict" dictionary
            "--non-interactive"
            "--exact-search"
            "--json-output"
            word)))
      (unless (eq status 0)
        (error "SDCV failed with status %s" status))
      (let ((output (string-trim (buffer-string))))
        (unless (string-empty-p output)
          (json-parse-string output :object-type 'alist :array-type 'list))))))

(defvar mdx-dict-entry-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'mdx-dict-follow-entry)
    (define-key map [mouse-1] #'mdx-dict-follow-entry-mouse)
    map)
  "Keymap placed on MDX cross-reference links.")

(defvar mdx-dict-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "s") #'mdx-dict-search)
    (define-key map (kbd "g") #'mdx-dict-refresh)
    (define-key map (kbd "b") #'mdx-dict-history-backward)
    (define-key map (kbd "f") #'mdx-dict-history-forward)
    map)
  "Keymap for `mdx-dict-mode'.")

(define-derived-mode mdx-dict-mode special-mode "MDict"
  "Major mode for browsing results from `mdx-dict-search'."
  (setq-local truncate-lines nil)
  (visual-line-mode 1))

(defun mdx-dict--word-at-point ()
  "Return the active region or the word at point without properties."
  (if (use-region-p)
      (string-trim
       (buffer-substring-no-properties (region-beginning) (region-end)))
    (thing-at-point 'word t)))

(defun mdx-dict--shortest-candidate (candidates)
  "Return the shortest string in CANDIDATES, breaking ties alphabetically."
  (seq-reduce
   (lambda (best candidate)
     (if (or (null best)
             (< (length candidate) (length best))
             (and (= (length candidate) (length best))
                  (string-lessp candidate best)))
         candidate
       best))
   candidates nil))

(defun mdx-dict--fuzzy-candidate (word candidates)
  "Return the closest sensible match to WORD from CANDIDATES."
  (let* ((query (downcase word))
         (query-length (length query))
         (maximum-distance (max 2 (ceiling query-length 3.0)))
         best
         best-distance)
    (dolist (candidate candidates best)
      (let* ((folded (downcase candidate))
             (length-difference (abs (- (length folded) query-length))))
        (when (and (<= length-difference maximum-distance)
                   (> query-length 0)
                   (> (length folded) 0)
                   (= (aref folded 0) (aref query 0)))
          (let ((distance (string-distance query folded)))
            (when (and (<= distance maximum-distance)
                       (or (null best-distance)
                           (< distance best-distance)
                           (and (= distance best-distance)
                                (< (length candidate) (length best)))
                           (and (= distance best-distance)
                                (= (length candidate) (length best))
                                (string-lessp candidate best))))
              (setq best candidate
                    best-distance distance))))))))

(defun mdx-dict--best-match (word)
  "Return the best configured MDX headword for WORD.
Prefer an exact or punctuation-insensitive match, then a prefix match, and
finally a nearby spelling.  Return WORD unchanged when no sensible match is
available."
  (let ((dictionaries (mdx-dict--available-dictionaries)))
    (or
     (catch 'match
       (dolist (dictionary dictionaries)
         (when-let* ((key (mdx-dict--resolve-key word (cdr dictionary))))
           (throw 'match key))))
     (let* ((completion-ignore-case t)
            (completion-styles '(basic))
            (candidates (mdx-dict--completion-candidates))
            (prefix-matches (all-completions word candidates)))
       (or (mdx-dict--shortest-candidate prefix-matches)
           (mdx-dict--fuzzy-candidate word candidates)))
     word)))

(defun mdx-dict--source-enabled-p (source selected)
  "Return non-nil when SOURCE is enabled by SELECTED names.
Nil SELECTED means that every source is enabled."
  (or (null selected) (member source selected)))

(defun mdx-dict--entry-target (url)
  "Return a dictionary headword when URL is an entry link."
  (when (and url (string-match "\\`entry://\\([^#?]+\\)" url))
    (url-unhex-string (match-string 1 url))))

(defun mdx-dict--activate-entry-links (start end)
  "Make MDict entry links between START and END queryable in Emacs."
  (let ((position start))
    (while (< position end)
      (let* ((url (get-text-property position 'shr-url))
             (next (next-single-property-change
                    position 'shr-url nil end))
             (target (mdx-dict--entry-target url)))
        (when target
          (add-text-properties
           position next
           `(mdx-dict-entry ,target
             keymap ,mdx-dict-entry-map
             mouse-face highlight
             help-echo ,(format "Look up %s" target)
             follow-link t)))
        (setq position next)))))

(defun mdx-dict-follow-entry ()
  "Look up the MDX cross-reference at point."
  (interactive)
  (let ((target
         (or (get-text-property (point) 'mdx-dict-entry)
             (and (> (point) (point-min))
                  (get-text-property (1- (point)) 'mdx-dict-entry)))))
    (unless target
      (user-error "No dictionary cross-reference at point"))
    (mdx-dict-search target mdx-dict--current-sources)))

(defun mdx-dict-follow-entry-mouse (event)
  "Look up the MDX cross-reference clicked in mouse EVENT."
  (interactive "e")
  (let* ((position (event-end event))
         (window (posn-window position)))
    (when (windowp window)
      (select-window window))
    (goto-char (posn-point position))
    (mdx-dict-follow-entry)))

(defun mdx-dict--insert-heading (heading)
  "Insert a dictionary section HEADING."
  (insert (propertize heading 'face '(:weight bold :height 1.15)) "\n"))

(defun mdx-dict--insert-mdx-result (word name file)
  "Insert WORD's result from MDX FILE under dictionary NAME.
Return non-nil when the dictionary contains an entry."
  (when-let* ((result (mdx-dict--lookup-entry word file)))
    (mdx-dict--insert-heading name)
    (let ((resolved (plist-get result :key)))
      (unless (string-equal-ignore-case word resolved)
        (insert (format "Redirected to: %s\n\n" resolved))))
    (let* ((html (plist-get result :html))
           (dom
            (with-temp-buffer
              (insert html)
              (libxml-parse-html-region (point-min) (point-max))))
           (start (point))
           (shr-inhibit-images t)
           (shr-use-colors nil)
           (shr-use-fonts nil)
           (shr-width (max 40 (- (window-body-width) 4))))
      (shr-insert-document dom)
      (mdx-dict--activate-entry-links start (point)))
    (insert "\n\n")
    t))

(defun mdx-dict--insert-sdcv-result (word dictionary)
  "Insert WORD's definitions from StarDict DICTIONARY.
Return non-nil when the dictionary contains an entry."
  (when-let* ((entries (mdx-dict--sdcv-query word dictionary)))
    (mdx-dict--insert-heading dictionary)
    (dolist (entry entries)
      (when-let* ((definition (alist-get 'definition entry)))
        (insert (string-trim definition) "\n\n")))
    t))

(defun mdx-dict--insert-results (word sources)
  "Insert dictionary results for WORD from selected SOURCES.
Return the number of dictionaries with an entry."
  (insert (propertize (format "%s\n" word)
                      'face '(:weight bold :height 1.35))
          (propertize
           "s: look up word at point    b/f: history    g: refresh\n\n"
           'face 'shadow))
  (let ((matches 0))
    (dolist (dictionary (mdx-dict--available-dictionaries))
      (let ((name (car dictionary))
            (file (cdr dictionary)))
        (when (mdx-dict--source-enabled-p name sources)
          (condition-case error-data
              (when (mdx-dict--insert-mdx-result word name file)
                (setq matches (1+ matches)))
            (error
             (mdx-dict--insert-heading name)
             (insert (format "Lookup failed: %s\n\n"
                             (error-message-string error-data))))))))
    (dolist (dictionary mdx-dict-sdcv-dictionaries)
      (when (mdx-dict--source-enabled-p dictionary sources)
        (condition-case error-data
            (when (mdx-dict--insert-sdcv-result word dictionary)
              (setq matches (1+ matches)))
          (error
           (mdx-dict--insert-heading dictionary)
           (insert (format "Lookup failed: %s\n\n"
                           (error-message-string error-data)))))))
    (when (zerop matches)
      (insert (format "No dictionary entries found for %s.\n" word)))
    matches))

(defun mdx-dict--current-spec ()
  "Return the current result buffer lookup specification."
  (and mdx-dict--current-word
       (list mdx-dict--current-word mdx-dict--current-sources)))

(defun mdx-dict--display (word sources &optional preserve-history)
  "Display WORD from SOURCES in `mdx-dict-result-buffer'.
When PRESERVE-HISTORY is non-nil, do not change navigation stacks."
  (let ((buffer (get-buffer-create mdx-dict-result-buffer))
        matches)
    (with-current-buffer buffer
      (unless (derived-mode-p 'mdx-dict-mode)
        (mdx-dict-mode))
      (let ((new-spec (list word sources))
            (old-spec (mdx-dict--current-spec)))
        (unless preserve-history
          (when (and old-spec (not (equal old-spec new-spec)))
            (push old-spec mdx-dict--back-history))
          (setq mdx-dict--forward-history nil))
        (setq mdx-dict--current-word word
              mdx-dict--current-sources sources))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq matches (mdx-dict--insert-results word sources))
        (goto-char (point-min))))
    (display-buffer buffer)
    (message "MDict: %d %s matched %s"
             matches (if (= matches 1) "dictionary" "dictionaries") word)
    buffer))

;;;###autoload
(defun mdx-dict-search (word &optional sources)
  "Look up WORD in local MDX and StarDict dictionaries.
When called interactively, use the active region or word at point without
prompting and display the best matching dictionary headword.  By default query
every configured source.  SOURCES is an optional list of dictionary names."
  (interactive
   (list (or (mdx-dict--word-at-point)
             (user-error "No word at point"))))
  (when (string-empty-p word)
    (user-error "Dictionary word cannot be empty"))
  (let ((match (mdx-dict--best-match (string-trim word))))
    (unless (string-equal word match)
      (message "MDict: using best match %s" match))
    (mdx-dict--display match sources)))

(defun mdx-dict-refresh ()
  "Redisplay the current dictionary lookup."
  (interactive)
  (unless mdx-dict--current-word
    (user-error "No current dictionary lookup"))
  (mdx-dict--display mdx-dict--current-word mdx-dict--current-sources t))

(defun mdx-dict-history-backward ()
  "Return to the previous dictionary lookup."
  (interactive)
  (unless mdx-dict--back-history
    (user-error "No previous dictionary lookup"))
  (let ((target (pop mdx-dict--back-history)))
    (when-let* ((current (mdx-dict--current-spec)))
      (push current mdx-dict--forward-history))
    (mdx-dict--display (car target) (cadr target) t)))

(defun mdx-dict-history-forward ()
  "Advance to the next dictionary lookup."
  (interactive)
  (unless mdx-dict--forward-history
    (user-error "No later dictionary lookup"))
  (let ((target (pop mdx-dict--forward-history)))
    (when-let* ((current (mdx-dict--current-spec)))
      (push current mdx-dict--back-history))
    (mdx-dict--display (car target) (cadr target) t)))

(defun mdx-dict-synosaurus--clean-synonym (synonym)
  "Remove dictionary annotations from SYNONYM."
  (let ((clean (string-trim synonym)))
    (setq clean (replace-regexp-in-string "\\`[|*]+" "" clean))
    (setq clean (replace-regexp-in-string
                 "[[:space:]]+[0-9]+$" "" clean))
    (setq clean (replace-regexp-in-string "[[:space:]]+" " " clean))
    (setq clean (string-trim-right clean "[[:space:].]+"))
    (unless (string-empty-p clean)
      clean)))

(defun mdx-dict-synosaurus--candidate-group (text word)
  "Parse synonym list TEXT and remove WORD from the candidates."
  (setq text (replace-regexp-in-string
              "\\`[[:space:]]*synonyms:?[[:space:]]*" "" text t))
  (delete-dups
   (delq nil
         (mapcar
          (lambda (candidate)
            (let ((clean (mdx-dict-synosaurus--clean-synonym candidate)))
              (unless (and clean (string-equal-ignore-case clean word))
                clean)))
          (split-string text "[,;]" t)))))

(defun mdx-dict-synosaurus--mdx-groups (word)
  "Return direct synonym groups for WORD from the Merriam-Webster MDX."
  (when-let* ((entry (mdx-dict-synosaurus--mdx-entry word)))
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
                       (mdx-dict-synosaurus--candidate-group
                        (dom-inner-text container) word)))
            (when group (push group groups))))
        ;; Some dictionary-only entries have a shorter Synonyms field.
        (unless groups
          (dolist (dl (dom-by-tag tree 'dl))
            (when (dom-by-class dl "ltp")
              (when-let* ((dd (car (dom-by-tag dl 'dd)))
                          (group
                           (mdx-dict-synosaurus--candidate-group
                            (dom-inner-text dd) word)))
                (when group (push group groups))))))
        (nreverse groups)))))

(defun mdx-dict-synosaurus--soule-heading-p (line)
  "Return non-nil when LINE is a Soule part-of-speech or sense heading."
  (or (string-match-p "\\`[0-9]+\\.\\'" line)
      (string-match-p
       "\\`[IVXLCDM]+\\.\\(?:[[:space:]]+[anv]\\.\\(?:[[:space:]]+[anv]\\.\\)?\\)?\\'"
       line)
      (string-match-p
       "\\`[anv]\\.\\(?:[[:space:]]+[anv]\\.\\)?\\'" line)))

(defun mdx-dict-synosaurus--soule-groups (word)
  "Return synonym groups for WORD from the local Soule StarDict data."
  (let ((entries (mdx-dict--sdcv-query
                  word mdx-dict-synosaurus-soule-dictionary))
        groups)
    (dolist (entry entries (nreverse groups))
      (dolist (line (split-string
                     (alist-get 'definition entry) "\n" t "[[:space:]]+"))
        (unless (mdx-dict-synosaurus--soule-heading-p line)
          (let ((group (mdx-dict-synosaurus--candidate-group line word)))
            (when group (push group groups))))))))

(defun mdx-dict-synosaurus-backend (word)
  "Return synonyms for WORD from Merriam-Webster MDX, then Soule."
  (let ((mdx-groups (mdx-dict-synosaurus--mdx-groups word)))
    (if mdx-groups
        (progn
          (message "Synosaurus: using Merriam-Webster Collegiate Thesaurus (2015)")
          mdx-groups)
      (let ((soule-groups (mdx-dict-synosaurus--soule-groups word)))
        (if soule-groups
            (message "Synosaurus: using Soule's Dictionary (Merriam-Webster had no synonyms)")
          (message "Synosaurus: no synonyms in Merriam-Webster or Soule's Dictionary"))
        soule-groups))))

(provide 'mdx-dict-and-synosaurus)

;;; mdx-dict-and-synosaurus.el ends here
