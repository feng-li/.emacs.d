;;; notmuch-custom.el --- Local enhancements for Notmuch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (company "1.0.0") (ivy "0.15.0") (notmuch "0.38"))
;; Keywords: mail, completion, multilingual

;;; Commentary:

;; Local Notmuch enhancements:
;;
;; - saved searches generated from the active Notmuch profile, plus a
;;   global starred view;
;; - address completion using literal text, full Pinyin, or Pinyin initials;
;; - compact, expandable To and Cc headers;
;; - prose checking limited to the subject and newly written message text;
;; - attachment opening through the desktop default application;
;; - automatic, display-only reflow of hard-wrapped plain-text messages;
;; - recipients instead of senders in saved Sent-folder views;
;; - omission of a signature already present in quoted reply text;
;; - canonical reply and forward subject markers;
;; - CRLF normalization in inline forwarded messages;
;; - post-send IMAP reply/forward flags that Thunderbird understands;
;; - mirroring of IMAP stars onto Notmuch's `imap-starred' tag; and
;; - coexistence of address completion in headers and word completion in bodies.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'company)
(require 'ivy-pinyin-search)
(require 'mail-extr)
(require 'network-stream)
(require 'nnheader)
(require 'notmuch)
(require 'notmuch-address)
(require 'notmuch-company)
(require 'notmuch-message)
(require 'notmuch-show)
(require 'subr-x)
(require 'seq)
(require 'utf7)

(defgroup notmuch-custom nil
  "Local enhancements for Notmuch."
  :group 'notmuch)

;;; Saved searches

(defcustom notmuch-custom-unified-folders nil
  "Folder names for which to add unified saved searches across accounts.
Nil disables generation; t includes every shared folder name; a list of
strings selects names such as \"Papers\".  Names match case-sensitively.
Profile query names must follow NUMBER-ACCOUNT--FOLDER, optionally with
more -- separated subfolders.  The full folder path must match across
at least two accounts.  Account-specific searches remain available.
Rebuild `notmuch-saved-searches' after changing this option."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "All shared folders" t)
                 (repeat :tag "Selected folders" string))
  :group 'notmuch-custom)

(defcustom notmuch-custom-global-starred-search t
  "When non-nil, add an \"All Starred\" saved search across accounts.
Server stars carry the `imap-starred' tag, populated directly from
IMAP by `notmuch-custom-sync-imap-starred'.  Rebuild
`notmuch-saved-searches' after changing this option."
  :type 'boolean
  :group 'notmuch-custom)

(defun notmuch-custom--unified-searches (query-names)
  "Build unified saved searches from ordered profile QUERY-NAMES."
  (let ((groups (make-hash-table :test #'equal))
        order searches)
    (dolist (query-name query-names)
      (let* ((name (replace-regexp-in-string "\\`[0-9]+-" "" query-name))
             (components (split-string name "--"))
             (account (car components))
             (folder (mapconcat #'identity (cdr components) "--"))
             (label (notmuch-custom-query-display-name folder)))
        (when (and (cdr components)
                   (or (eq notmuch-custom-unified-folders t)
                       (member label notmuch-custom-unified-folders)))
          (unless (gethash folder groups)
            (push folder order))
          (puthash folder (cons (cons account query-name)
                                (gethash folder groups)) groups))))
    (dolist (folder (nreverse order))
      (let ((members (nreverse (gethash folder groups))))
        (when (> (length (delete-dups (mapcar #'car members))) 1)
          (push (list :name (concat "All "
                                    (notmuch-custom-query-display-name folder))
                      :query (concat "("
                                     (mapconcat
                                      (lambda (member)
                                        (concat "query:" (cdr member)))
                                      members " or ")
                                     ")")
                      :search-type 'unthreaded
                      :show-recipients
                      (let ((case-fold-search t))
                        (string-match-p "\\(?:\\`\\|--\\)sent\\'" folder)))
                searches))))
    (nreverse searches)))

(defun notmuch-custom-query-display-name (query-name)
  "Turn an ordered Notmuch QUERY-NAME into a display label.
The numeric prefix controls order, a hyphen becomes a space, and a
double hyphen becomes a folder separator."
  (let ((name (replace-regexp-in-string "\\`[0-9]+-" "" query-name)))
    (setq name (replace-regexp-in-string "--" " / " name))
    (replace-regexp-in-string "-" " " name)))

(defun notmuch-custom--starred-search ()
  "Build the global saved search for starred messages.
Configured accounts share the `imap-starred' tag, so unlike the
per-folder unified searches this needs no `query:' disjunction."
  (list :name "All Starred"
        :query "tag:imap-starred"
        :search-type 'unthreaded))

(defun notmuch-custom-saved-searches-from-profile ()
  "Build Emacs saved searches from the active profile's query.* entries."
  (let (query-names)
    (dolist (line (notmuch--process-lines notmuch-command "config" "list"))
      (when (string-match "\\`query\\.\\([^=]+\\)=" line)
        (push (match-string 1 line) query-names)))
    (setq query-names (sort query-names #'string-lessp))
    (append
     (notmuch-custom--unified-searches query-names)
     (and notmuch-custom-global-starred-search
          (list (notmuch-custom--starred-search)))
     (mapcar (lambda (query-name)
              (list :name (notmuch-custom-query-display-name query-name)
                    :query (concat "query:" query-name)
                    :search-type 'unthreaded
                    :show-recipients
                    (let ((case-fold-search t))
                      (string-match-p "--sent\\'" query-name))))
             query-names))))

;;; Sent-folder correspondents

(defun notmuch-custom--sent-folder-view-p ()
  "Return non-nil when the current tree buffer is a saved Sent view."
  (and (boundp 'notmuch-tree-basic-query)
       (stringp notmuch-tree-basic-query)
       (cl-some
        (lambda (search)
          (and (notmuch-saved-search-get search :show-recipients)
               (equal notmuch-tree-basic-query
                      (notmuch-saved-search-get search :query))))
        notmuch-saved-searches)))

(defun notmuch-custom--message-recipient-names (message)
  "Return the display names of all recipients in MESSAGE.
MESSAGE is a Notmuch message plist as used by `notmuch-tree-mode'."
  (let* ((headers (plist-get message :headers))
         (addresses
          (string-join
           (delq nil (mapcar (lambda (field)
                               (let ((value (plist-get headers field)))
                                 (and (stringp value)
                                      (not (string-empty-p value))
                                      value)))
                             '(:To :Cc :Bcc)))
           ", "))
         (recipients
          (and (not (string-empty-p addresses))
               (mail-extract-address-components addresses t))))
    (if recipients
        (mapconcat (lambda (recipient)
                     (or (car recipient) (cadr recipient)))
                   recipients ", ")
      "(no recipient)")))

(defun notmuch-custom--format-sent-folder-recipients
    (original-function field format-string message)
  "Use recipients for the author FIELD in a saved Sent-folder view.
Otherwise call ORIGINAL-FUNCTION with FORMAT-STRING and MESSAGE unchanged."
  (if (and (equal field "authors")
           (notmuch-custom--sent-folder-view-p))
      (let* ((recipients (notmuch-custom--message-recipient-names message))
             (width (length (format format-string "")))
             (face (if (plist-get message :match)
                       'notmuch-tree-match-author-face
                     'notmuch-tree-no-match-author-face)))
        (propertize
         (format format-string
                 (truncate-string-to-width recipients width nil nil "..."))
         'face face))
    (funcall original-function field format-string message)))

;;; Pinyin address completion

(defvar-local notmuch-custom-company-pinyin--last-prefix nil)

(defun notmuch-custom--company-pinyin-regexp (input)
  "Build a Pinyin regexp for simple Latin INPUT.
Return nil when INPUT contains characters unsuitable for Pinyin matching."
  (when (string-match-p "\\`[A-Za-z \\t]+\\'" input)
    (let* ((text (downcase input))
           (full (ivy-pinyin-search--full-regexp text))
           (initials (ivy-pinyin-search--initial-regexp text t))
           (regexps (delete-dups (delq nil (list full initials)))))
      (cond
       ((null regexps) nil)
       ((null (cdr regexps)) (car regexps))
       (t (concat "\\(?:" (mapconcat #'identity regexps "\\|") "\\)"))))))

(defun notmuch-custom--company-pinyin-matching (input)
  "Return cached Notmuch addresses matching literal or Pinyin INPUT."
  (let ((case-fold-search t)
        (literal-regexp (regexp-quote input))
        (pinyin-regexp (notmuch-custom--company-pinyin-regexp input))
        candidates)
    (maphash
     (lambda (candidate _value)
       (when (or (string-match-p literal-regexp candidate)
                 (and pinyin-regexp
                      (string-match-p pinyin-regexp candidate)))
         (push candidate candidates)))
     notmuch-address-completions)
    candidates))

(defun notmuch-custom--company-pinyin-harvest (input callback buffer)
  "Harvest all addresses, then call CALLBACK with matches for INPUT in BUFFER."
  (setq notmuch-address-last-harvest (float-time))
  (notmuch-address-harvest
   nil nil
   (lambda (_process event)
     (let ((finished (string= event "finished\n")))
       (if finished
           (progn
             (setq notmuch-address-full-harvest-finished t)
             (notmuch-address--save-address-hash))
         (setq notmuch-address-last-harvest 0))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (funcall callback
                    (and finished
                         (notmuch-custom--company-pinyin-matching input)))))))))

(defun notmuch-custom-company-pinyin (command &optional arg &rest ignored)
  "Complete Notmuch addresses using literal text or Pinyin.
COMMAND, ARG, and IGNORED follow the Company backend protocol."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'notmuch-custom-company-pinyin))
    (prefix
     (setq notmuch-custom-company-pinyin--last-prefix
           (notmuch-company 'prefix)))
    (candidates
     (if (not (eq notmuch-address-command 'internal))
         (apply #'notmuch-company command arg ignored)
       (if (notmuch-address--harvest-ready)
           (progn
             (notmuch-address-harvest-trigger)
             (notmuch-custom--company-pinyin-matching arg))
         (let ((buffer (current-buffer)))
           (cons :async
                 (lambda (callback)
                   (notmuch-custom--company-pinyin-harvest
                    arg callback buffer)))))))
    (match
     ;; Pinyin input has no literal span to highlight in a Chinese name.
     (if (and notmuch-custom-company-pinyin--last-prefix
              (string-match notmuch-custom-company-pinyin--last-prefix arg))
         (match-end 0)
       0))
    (post-completion
     (run-hook-with-args 'notmuch-address-post-completion-functions arg))
    (ignore-case t)
    (no-cache t)))

(defun notmuch-custom-company-setup ()
  "Configure Company for addresses in headers and words in message bodies."
  (setq-local company-backends
              '(notmuch-custom-company-pinyin
                (company-yasnippet company-dabbrev company-ispell :separate)
                company-files)))

;;; Prose checking scope

(defconst notmuch-custom--forward-start-regexp
  (concat
   "\\(?:"
   "[ \t]*-+[ \t]*"
   "\\(?:start of forwarded message\\|begin forwarded message"
   "\\|forwarded message\\|original message"
   "\\|回复的原邮件\\|回覆的原郵件\\|回復的原郵件"
   "\\|原始邮件\\|原始郵件\\|转发邮件\\|轉寄郵件\\)"
   "[ \t]*-+[ \t]*"
   "\\|[ \t]*begin forwarded message:[ \t]*"
   "\\)")
  "Regexp matching a line that starts an inline forwarded message.")

(defconst notmuch-custom--forward-end-regexp
  "[ \t]*-+[ \t]*end of forwarded message[ \t]*-+[ \t]*"
  "Regexp matching a line that ends an inline forwarded message.")

(defconst notmuch-custom--embedded-mail-header-regexp
  "[ \t]*\\(?:发件人\\|發件人\\|寄件人\\|寄件者\\)[ \t]*[:：]"
  "Regexp introducing an embedded Chinese mail header without a separator.")

(defconst notmuch-custom--reply-attribution-regexp
  (concat
   ".*\\(?:wrote\\|writes\\|写道\\|寫道\\)"
   "[ \t]*[:：][ \t]*")
  "Regexp matching a line that introduces quoted reply text.")

(defconst notmuch-custom--mml-marker-regexp
  "[ \t]*<#/?\\(?:part\\|multipart\\|secure\\|external\\)\\b.*"
  "Regexp matching a Message mode MML control line.")

(defun notmuch-custom--message-body-start ()
  "Return the first body position in the current Message buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward
         (concat "^" (regexp-quote mail-header-separator) "[ \t]*$")
         nil t)
        (progn
          (forward-line 1)
          (point))
      ;; This fallback also makes the filtering safe for unusual Message mode
      ;; buffers in which the visible header separator has been removed.
      (goto-char (point-min))
      (if (re-search-forward "^[ \t]*$" nil t)
          (progn
            (forward-line 1)
            (point))
        (point-max)))))

(defun notmuch-custom--subject-value-range (header-end)
  "Return the Subject value range before HEADER-END, or nil.
The range includes RFC-style continuation lines but not the `Subject:' label."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^Subject:[ \t]*" header-end t)
        (let ((start (point)))
          (forward-line 1)
          (while (and (< (point) header-end)
                      (looking-at-p "[ \t]"))
            (forward-line 1))
          (cons start (min (point) header-end)))))))

(defun notmuch-custom--quoted-line-p ()
  "Return non-nil when the current line is quoted reply text."
  (looking-at-p "[ \t]*>"))

(defun notmuch-custom--reply-attribution-line-p ()
  "Return non-nil when the current line introduces quoted reply text."
  (and (let ((case-fold-search t))
         (looking-at-p notmuch-custom--reply-attribution-regexp))
       (save-excursion
         (forward-line 1)
         (while (and (not (eobp))
                     (looking-at-p "[ \t]*$"))
           (forward-line 1))
         (notmuch-custom--quoted-line-p))))

(defun notmuch-custom--flycheck-message-checkable-ranges ()
  "Return buffer ranges that prose checkers should inspect.
Only the Subject value and newly written body text are included.  Other mail
headers, quoted replies, forwarding blocks, and MML control lines are omitted."
  (save-restriction
    (widen)
    (let* ((body-start (notmuch-custom--message-body-start))
           (subject-range
            (notmuch-custom--subject-value-range body-start))
           (case-fold-search t)
           (in-forward nil)
           ranges)
      (when subject-range
        (push subject-range ranges))
      (save-excursion
        (goto-char body-start)
        (while (< (point) (point-max))
          (let ((line-start (point))
                (line-end (progn (forward-line 1) (point))))
            (goto-char line-start)
            (cond
             ((looking-at-p notmuch-custom--forward-start-regexp)
              (setq in-forward t))
             (in-forward
              (when (looking-at-p notmuch-custom--forward-end-regexp)
                (setq in-forward nil)))
             ((or (notmuch-custom--quoted-line-p)
                  (notmuch-custom--reply-attribution-line-p)
                  (looking-at-p notmuch-custom--mml-marker-regexp)))
             (t
              (push (cons line-start line-end) ranges)))
            (goto-char line-end))))
      (nreverse ranges))))

(defun notmuch-custom--flycheck-message-position-checkable-p (position)
  "Return non-nil when POSITION is prose authored in this message."
  (cl-some (lambda (range)
             (and (<= (car range) position)
                  (< position (cdr range))))
           (notmuch-custom--flycheck-message-checkable-ranges)))

(defun notmuch-custom--flycheck-filter-buffer-text (text)
  "Mask non-authored mail text in checker input TEXT.
Newlines and string length are preserved so diagnostics still map to the
correct positions in the composition buffer."
  (if (not (derived-mode-p 'notmuch-message-mode))
      text
    (save-restriction
      (widen)
      ;; Both checker adapters normally return the entire buffer.  Avoid
      ;; applying full-buffer offsets if a future adapter supplies a slice.
      (if (/= (length text) (- (point-max) (point-min)))
          text
        (let ((filtered (copy-sequence text))
              (origin (point-min)))
          (cl-loop for index from 0 below (length filtered)
                   unless (eq (aref filtered index) ?\n)
                   do (aset filtered index ?\s))
          (dolist (range (notmuch-custom--flycheck-message-checkable-ranges))
            (let ((start (- (car range) origin))
                  (end (- (cdr range) origin)))
              (cl-loop for index from start below end
                       do (aset filtered index (aref text index)))))
          filtered)))))

(defun notmuch-custom--filter-languagetool-results (results)
  "Keep only LanguageTool RESULTS for authored message text."
  (if (not (derived-mode-p 'notmuch-message-mode))
      results
    (cl-remove-if-not
     (lambda (result)
       (notmuch-custom--flycheck-message-position-checkable-p (car result)))
     results)))

(defun notmuch-custom--vale-ignore-non-authored-text
    (original-function alert buffer)
  "Ignore Vale ALERT outside authored text in a Notmuch message.
ORIGINAL-FUNCTION applies Vale's other exclusion rules in BUFFER."
  (or (funcall original-function alert buffer)
      (with-current-buffer buffer
        (and (derived-mode-p 'notmuch-message-mode)
             (save-restriction
               (widen)
               (let* ((line (or (alist-get 'Line alert) 1))
                      (span (alist-get 'Span alert))
                      (column (or (car-safe span) 1))
                      (position
                       (flycheck-line-column-to-position line column)))
                 (not
                  (notmuch-custom--flycheck-message-position-checkable-p
                   position))))))))

(defun notmuch-custom--install-languagetool-filter ()
  "Install Notmuch composition filtering for LanguageTool."
  (unless (advice-member-p #'notmuch-custom--flycheck-filter-buffer-text
                           'flycheck-languagetool--buffer-text)
    (advice-add 'flycheck-languagetool--buffer-text :filter-return
                #'notmuch-custom--flycheck-filter-buffer-text))
  (unless (advice-member-p #'notmuch-custom--filter-languagetool-results
                           'flycheck-languagetool--check-all)
    (advice-add 'flycheck-languagetool--check-all :filter-return
                #'notmuch-custom--filter-languagetool-results)))

(defun notmuch-custom--install-vale-filter ()
  "Install Notmuch composition filtering for Vale."
  (unless (advice-member-p #'notmuch-custom--flycheck-filter-buffer-text
                           'flycheck-vale--buffer-text)
    (advice-add 'flycheck-vale--buffer-text :filter-return
                #'notmuch-custom--flycheck-filter-buffer-text))
  (unless (advice-member-p #'notmuch-custom--vale-ignore-non-authored-text
                           'flycheck-vale--ignored-alert-p)
    (advice-add 'flycheck-vale--ignored-alert-p :around
                #'notmuch-custom--vale-ignore-non-authored-text)))

;;; Compact address headers

(defcustom notmuch-custom-address-header-limit 4
  "Maximum number of addresses displayed in a Notmuch To or Cc header."
  :type 'integer
  :group 'notmuch-custom)

(defun notmuch-custom--format-address (components)
  "Format parsed mail address COMPONENTS for display."
  (pcase-let ((`(,name ,address) components))
    (cond
     ((and name address) (format "%s <%s>" name address))
     (address address)
     (name name)
     (t ""))))

(defun notmuch-custom--insert-collapsed-address-button (addresses count)
  "Insert a button representing COUNT omitted ADDRESSES."
  (insert-text-button
   (format "… (%d more)" count)
   'action #'notmuch-custom--expand-address-header
   'follow-link t
   'help-echo "Show all recipients"
   'notmuch-custom-omitted-count count
   'notmuch-custom-omitted-addresses addresses))

(defun notmuch-custom--expand-address-header (button)
  "Replace the truncation BUTTON with its omitted addresses and a hide button."
  (let ((start (button-start button))
        (end (button-end button))
        (addresses (button-get button 'notmuch-custom-omitted-addresses))
        (count (button-get button 'notmuch-custom-omitted-count))
        (inhibit-read-only t))
    (goto-char start)
    (delete-region start end)
    (let ((expanded-start (copy-marker start)))
      (insert addresses " ")
      (insert-text-button
       "[hide extra addresses]"
       'action #'notmuch-custom--collapse-address-header
       'follow-link t
       'help-echo "Hide extra recipients"
       'notmuch-custom-expanded-start expanded-start
       'notmuch-custom-omitted-count count
       'notmuch-custom-omitted-addresses addresses))))

(defun notmuch-custom--collapse-address-header (button)
  "Collapse the extra addresses preceding BUTTON."
  (let* ((marker (button-get button 'notmuch-custom-expanded-start))
         (start (marker-position marker))
         (end (button-end button))
         (addresses (button-get button 'notmuch-custom-omitted-addresses))
         (count (button-get button 'notmuch-custom-omitted-count))
         (inhibit-read-only t))
    (set-marker marker nil)
    (delete-region start end)
    (goto-char start)
    (notmuch-custom--insert-collapsed-address-button addresses count)))

(defun notmuch-custom--insert-truncated-address-header
    (original-function header header-value)
  "Call ORIGINAL-FUNCTION, abbreviating a long address HEADER-VALUE."
  (let ((addresses
         (when (member header '("To" "Cc"))
           (condition-case nil
               (mail-extract-address-components header-value t)
             (error nil)))))
    (if (or (null addresses)
            (<= (length addresses) notmuch-custom-address-header-limit))
        (funcall original-function header header-value)
      (let* ((shown
              (cl-subseq addresses 0 notmuch-custom-address-header-limit))
             (omitted
              (nthcdr notmuch-custom-address-header-limit addresses))
             (shown-text
              (mapconcat #'notmuch-custom--format-address shown ", "))
             (omitted-text
              (mapconcat #'notmuch-custom--format-address omitted ", ")))
        (insert header ": " (notmuch-sanitize shown-text) ", ")
        (notmuch-custom--insert-collapsed-address-button
         (notmuch-sanitize omitted-text) (length omitted))
        (insert "\n")))))

;;; Simplified HTML display

(defun notmuch-custom--render-simple-html (render &rest arguments)
  "Call RENDER with ARGUMENTS using theme colors and the default font.
Scope these settings to Notmuch's HTML rendering.  Links, emphasis, and
tables are still rendered by SHR."
  (require 'shr)
  (let ((shr-use-colors nil)
        (shr-use-fonts nil))
    (apply render arguments)))

;;; Plain-text display

(defun notmuch-custom--decode-unlabeled-chinese-text
    (original msg part process-crypto &optional cache)
  "Recover unlabeled GB18030 plain text, regardless of sender.
Only retry when ORIGINAL returns replacement characters or raw bytes.
Preserve valid UTF-8.  Otherwise accept GB18030 only when it decodes
losslessly and contains Chinese characters.  Charset detection is
heuristic; explicitly labeled parts are always left alone."
  (let ((text (funcall original msg part process-crypto cache)))
    (if (and (equal (plist-get part :content-type) "text/plain")
             (not (plist-get part :content-charset))
             (string-match-p "[\ufffd\x3fff80-\x3fffff]" text))
        (let* ((raw (notmuch-get-bodypart-binary msg part process-crypto cache))
               (utf8 (decode-coding-string raw 'utf-8-unix))
               (decoded (decode-coding-string raw 'gb18030-unix)))
          (cond
           ((and (not (string-match-p "[\x3fff80-\x3fffff]" utf8))
                 (equal raw (encode-coding-string utf8 'utf-8-unix)))
            utf8)
           ((and (not (string-match-p "[\ufffd\x3fff80-\x3fffff]" decoded))
                 (string-match-p
                  "[\u3400-\u4dbf\u4e00-\u9fff\U00020000-\U000323af]" decoded)
                 (equal raw (encode-coding-string decoded 'gb18030-unix)))
            decoded)
           (t text)))
      text)))

(defun notmuch-custom--normalize-crlf (start end &optional bare-cr)
  "Normalize CRLF to LF between START and END.
BARE-CR controls lone carriage returns: `keep' preserves them, `remove'
deletes them, and nil converts them to LF.  Respect the current narrowing."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\r\n?" nil t)
        (cond
         ((or (= (length (match-string 0)) 2) (null bare-cr))
          (replace-match "\n" t t))
         ((eq bare-cr 'remove) (replace-match "" t t)))))))

(defun notmuch-custom-normalize-plain-text-newlines (_msg _depth)
  "Normalize line endings in the narrowed display, leaving stored mail intact."
  (notmuch-custom--normalize-crlf (point-min) (point-max)))

(defun notmuch-custom-decode-plain-text-entities (_msg _depth)
  "Decode stray HTML entities in the narrowed plain-text display.
Decode once, preserving line breaks, literal tags, and unknown entities.
Only entity tokens are parsed as HTML, never the message itself.  The
stored message is unchanged.  Run before wrapping and blank-line cleanup."
  (when (libxml-available-p)
    (require 'dom)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search nil))
        (while (re-search-forward
                "&\\(?:#[0-9]+\\|#[xX][0-9a-fA-F]+\\|[A-Za-z][A-Za-z0-9]*\\);"
                nil t)
          (let* ((entity (match-string-no-properties 0))
                 (decoded
                  (save-match-data
                    (with-temp-buffer
                      (insert "<html><body><p>" entity "</p></body></html>")
                      (dom-texts
                       (dom-by-tag
                        (libxml-parse-html-region (point-min) (point-max))
                        'p))))))
            (unless (or (string-empty-p decoded) (equal entity decoded))
              (replace-match
               (replace-regexp-in-string "\u00a0" " " decoded t t)
               t t))))))))

;;; External attachment opening

(defun notmuch-custom-open-part-with-default-application ()
  "Open the MIME part at point using the desktop default application."
  (interactive)
  (unless (executable-find "xdg-open")
    (user-error "Cannot find xdg-open"))
  (notmuch-show-apply-to-current-part-handle
   (lambda (handle)
     (let* ((directory (make-temp-file "notmuch-attachment-" t))
            (original-name (or (mm-handle-filename handle) "attachment"))
            (base-name (file-name-nondirectory original-name))
            (file (expand-file-name
                   (if (string-empty-p base-name) "attachment" base-name)
                   directory))
            (process-connection-type nil))
       (mm-save-part-to-file handle file)
       (let ((process (start-process "notmuch-xdg-open" nil
                                     "xdg-open" file)))
         (set-process-query-on-exit-flag process nil))))))

;;; Reading hard-wrapped messages

(defcustom notmuch-custom-reflow-on-display t
  "When non-nil, reflow every message automatically as it is displayed.
This performs the reflowing of `notmuch-custom-reflow-current-message',
bound to W in Notmuch show buffers: ordinary prose is filled to
`notmuch-custom-reflow-width' columns, and quoted, structured,
signature, and forwarded text is left alone.  Only the Notmuch display
buffer is changed.  Set this to nil and refresh with g to see the
original wrapping again."
  :type 'boolean
  :group 'notmuch-custom)

(defcustom notmuch-custom-reflow-width 100
  "Column that reflowing fills ordinary prose to.
Notmuch's own washing already wraps text/plain parts to
`notmuch-wash-wrap-lines-length' columns as they are inserted, so keep
this wider than that for reflowing to have a visible effect; nil
follows the wash width instead."
  :type '(choice (const :tag "Follow wash width" nil)
                 (integer :tag "Columns"))
  :group 'notmuch-custom)

(defun notmuch-custom--plain-text-part-regions ()
  "Return visible inline text/plain regions in the current message."
  (let* ((extent (notmuch-show-message-extent))
         (position (car extent))
         (limit (cdr extent))
         regions)
    (while (< position limit)
      (let* ((part (get-text-property position :notmuch-part))
             (next (or (next-single-property-change
                        position :notmuch-part nil limit)
                       limit))
             (mime-type (and part (plist-get part :computed-type))))
        (when (and (stringp mime-type)
                   (string-match-p "\\`text/plain\\(?:;\\|\\'\\)" mime-type)
                   (not (plist-get part :filename)))
          (let ((start position))
            ;; A secondary MIME part can include its selector button in the
            ;; property range.  Reflow only the content following that button.
            (when-let* ((button (button-at start))
                        ((<= (button-end button) next)))
              (setq start (button-end button))
              (when (eq (char-after start) ?\n)
                (setq start (1+ start))))
            (when (< start next)
              (push (cons start next) regions))))
        (setq position (if (> next position) next (1+ position)))))
    (nreverse regions)))

(defun notmuch-custom--strip-thread-indent (line indent)
  "Remove at most INDENT leading spaces from LINE."
  (let ((count 0)
        (line-length (length line)))
    (while (and (< count indent)
                (< count line-length)
                (eq (aref line count) ?\s))
      (setq count (1+ count)))
    (substring line count)))

(defun notmuch-custom--structured-display-line-p (line)
  "Return non-nil when LINE should not be joined with prose around it."
  (or (string-empty-p line)
      (string-match-p "\\`[ \t]" line)
      (string-match-p "\\`>" line)
      ;; Notmuch's citation, original-message, and part buttons all
      ;; begin with a bracket.
      (string-match-p "\\`\\[" line)
      (string-match-p
       "\\`\\(?:[-+*]\\|[[:digit:]]+[.)]\\)[ \t]+" line)
      (string-match-p "\\`\\(?:|\\|```\\|~~~\\)" line)
      (string-match-p "\\`[-_=]\\{3,\\}[ \t]*\\'" line)))

(defun notmuch-custom--reflowable-blocks (start end indent)
  "Return ordinary prose blocks between START and END.
INDENT is the indentation Notmuch added for the thread depth.  Quotations,
lists, indented text, signatures, and forwarded-message blocks are omitted."
  (let ((case-fold-search t)
        (in-forward nil)
        (in-signature nil)
        block-start
        blocks)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-start (point))
               (line-end (min (progn (forward-line 1) (point)) end))
               (line
                (notmuch-custom--strip-thread-indent
                 (buffer-substring-no-properties
                  line-start
                  (save-excursion
                    (goto-char line-start)
                    (line-end-position)))
                 indent))
               (forward-start
                (string-match-p
                 (concat "\\`\\(?:" notmuch-custom--forward-start-regexp
                         "\\|" notmuch-custom--embedded-mail-header-regexp "\\)")
                 line))
               (forward-end
                (string-match-p
                 (concat "\\`" notmuch-custom--forward-end-regexp "\\'")
                 line))
               (signature-start (string-match-p "\\`--[ \t]*\\'" line))
               (structured
                (or in-forward in-signature forward-start signature-start
                    (notmuch-custom--structured-display-line-p line))))
          (if structured
              (when block-start
                (push (cons block-start line-start) blocks)
                (setq block-start nil))
            (unless block-start
              (setq block-start line-start)))
          (when forward-start
            (setq in-forward t))
          (when (and in-forward forward-end)
            (setq in-forward nil))
          (when signature-start
            (setq in-signature t))
          (goto-char line-end)))
      (when block-start
        (push (cons block-start end) blocks)))
    (nreverse blocks)))

(defun notmuch-custom--reflow-message-at-point ()
  "Reflow ordinary prose in the message at point to the configured width.
Only inline text/plain parts are changed, and only in the Notmuch display
buffer.  Quoted, structured, signature, and forwarded text is left alone.
Return the width reflowed to, or nil when nothing was reflowable."
  (let* ((width (or notmuch-custom-reflow-width
                    (if (numberp notmuch-wash-wrap-lines-length)
                        notmuch-wash-wrap-lines-length
                      80)))
         (depth (or (notmuch-show-get-depth) 0))
         (indent (if notmuch-show-indent-content
                     (* depth notmuch-show-indent-messages-width)
                   0))
         blocks)
    (dolist (region (notmuch-custom--plain-text-part-regions))
      (setq blocks
            (nconc blocks
                   (notmuch-custom--reflowable-blocks
                    (car region) (cdr region) indent))))
    (when blocks
      ;; Work from the bottom upward so earlier buffer positions remain stable.
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (fill-column (+ width indent))
            (fill-prefix (make-string indent ?\s))
            (adaptive-fill-mode nil)
            (sentence-end-double-space nil))
        (with-silent-modifications
          (dolist (block (reverse blocks))
            (fill-region-as-paragraph (car block) (cdr block)))))
      width)))

(defun notmuch-custom-reflow-current-message ()
  "Reflow ordinary prose in the displayed message to `notmuch-custom-reflow-width'.
Only inline text/plain parts are changed, and only in the Notmuch display
buffer.  Quoted, structured, signature, and forwarded text is left alone.
Refresh the buffer with `notmuch-show-refresh-view' to restore its
rendering, or to see the original wrapping after disabling
`notmuch-custom-reflow-on-display'."
  (interactive)
  (unless (derived-mode-p 'notmuch-show-mode)
    (user-error "This command is only available while reading Notmuch mail"))
  (let ((width (notmuch-custom--reflow-message-at-point)))
    (if width
        (message "Reflowed current message to %d columns; press g to restore"
                 width)
      (user-error "The current message has no reflowable plain-text prose"))))

(defun notmuch-custom--reflow-displayed-messages ()
  "Reflow every message in the current Notmuch show buffer.
A message that cannot be reflowed is left untouched."
  (notmuch-show-mapc
   (lambda ()
     (ignore-errors (notmuch-custom--reflow-message-at-point)))))

(defun notmuch-custom--reflow-after-build (&rest _)
  "Reflow displayed messages after a Notmuch show buffer is built.
A buffer that was built without any message, or cannot be walked for
some other reason, is left alone rather than disturbing the display
that caused it."
  (when (and notmuch-custom-reflow-on-display
             (derived-mode-p 'notmuch-show-mode))
    (ignore-errors
      (notmuch-custom--reflow-displayed-messages))))

;;; Forwarded message cleanup

(defconst notmuch-custom--reply-subject-word-regexp
  "\\(?:re\\|reply\\|sv\\|回复\\|回覆\\|答复\\|答覆\\|回信\\)"
  "Regexp matching a reply word at the start of a subject marker.")

(defconst notmuch-custom--forward-subject-word-regexp
  "\\(?:fw\\|fwd\\|forward\\|转发\\|轉發\\|转寄\\|轉寄\\)"
  "Regexp matching a forward word at the start of a subject marker.")

(defun notmuch-custom--subject-marker-regexp (word-regexp)
  "Return a complete subject-marker regexp based on WORD-REGEXP."
  (concat word-regexp
          ;; Accept thread counters used by forms such as Re[2] and Re(2).
          "\\(?:([0-9]+)\\|\\[[0-9]+\\]\\|\\^[0-9]+\\)?"
          ;; Accept ASCII and common full-width Unicode colons.
          "[[:space:]\u00a0]*[:：﹕꞉][[:space:]\u00a0]*"))

(defun notmuch-custom--strip-subject-markers (subject)
  "Remove every consecutive reply or forward marker from SUBJECT."
  (let* ((case-fold-search t)
         (reply (notmuch-custom--subject-marker-regexp
                 notmuch-custom--reply-subject-word-regexp))
         (forward (notmuch-custom--subject-marker-regexp
                   notmuch-custom--forward-subject-word-regexp))
         (prefix (concat "\\`[[:space:]\u00a0]*\\(?:"
                         reply "\\|" forward "\\)+"))
         (cleaned subject))
    (while (string-match prefix cleaned)
      (setq cleaned (substring cleaned (match-end 0))))
    (string-trim cleaned)))

(defun notmuch-custom--canonical-subject (subject kind)
  "Return SUBJECT with one canonical marker appropriate for KIND.
KIND is either `reply' or `forward'."
  (let ((base (notmuch-custom--strip-subject-markers (or subject "")))
        (marker (pcase kind
                  ('reply "Re:")
                  ('forward "Fwd:")
                  (_ (error "Unknown subject kind: %S" kind)))))
    (if (string-empty-p base)
        marker
      (concat marker " " base))))

(defun notmuch-custom--normalize-current-reply-subject (&rest _ignored)
  "Replace repeated markers in the current Notmuch reply with one `Re:'."
  (when (derived-mode-p 'notmuch-message-mode)
    (when-let* ((subject (message-fetch-field "Subject")))
      (message-replace-header
       "Subject" (notmuch-custom--canonical-subject subject 'reply)))))

(defun notmuch-custom--configured-signature-text ()
  "Return the text of the configured Message-mode signature, or nil."
  (cond
   ((stringp message-signature) message-signature)
   ((and message-signature-file
         (file-readable-p message-signature-file))
    (with-temp-buffer
      (insert-file-contents message-signature-file)
      (buffer-string)))))

(defun notmuch-custom--normalized-signature-text (text)
  "Normalize line endings and trailing whitespace in signature TEXT."
  (string-trim
   (mapconcat #'string-trim-right
              (split-string
               (replace-regexp-in-string "\r\n?" "\n" text)
               "\n")
              "\n")))

(defun notmuch-custom--quoted-reply-text ()
  "Return the quoted lines in the current reply without quote prefixes."
  (save-excursion
    (message-goto-body)
    (let (lines)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match-p "\\`[ \t]*>" line)
            (while (string-match "\\`[ \t]*>[ \t]?" line)
              (setq line (substring line (match-end 0))))
            (push (string-trim-right line) lines)))
        (forward-line 1))
      (mapconcat #'identity (nreverse lines) "\n"))))

(defun notmuch-custom--remove-current-signature (signature)
  "Remove the unquoted current-message SIGNATURE and its separator.
Remove every bare separator even when the signature body has already
disappeared or Message mode inserted more than one separator."
  (save-excursion
    (message-goto-body)
    (let ((body-start (point))
          (signature (string-trim-right signature)))
      ;; Remove the unquoted signature body first.  A quoted copy cannot match
      ;; this multi-line string because every quoted line has its own `>'
      ;; prefix.
      (unless (string-empty-p signature)
        (while (search-forward signature nil t)
          (let ((start (match-beginning 0))
                (end (match-end 0)))
            (unless (save-excursion
                      (goto-char start)
                      (beginning-of-line)
                      (looking-at-p "[ \t]*>"))
              (delete-region start end)))))
      ;; Remove all unquoted separator-only lines.  Doing this independently
      ;; of the signature body also handles duplicate or orphan separators.
      (goto-char body-start)
      (while (re-search-forward "^--[ \t]*\\(?:\n\\|\\'\\)" nil t)
        (replace-match "" t t)))))

(defun notmuch-custom--omit-signature-already-quoted (&rest _ignored)
  "Omit the new signature when the quoted reply already contains its text."
  (when (derived-mode-p 'notmuch-message-mode)
    (when-let* ((signature (notmuch-custom--configured-signature-text))
                (needle (notmuch-custom--normalized-signature-text signature)))
      (unless (string-empty-p needle)
        (let ((quoted
               (notmuch-custom--normalized-signature-text
                (notmuch-custom--quoted-reply-text))))
          (when (string-match-p (regexp-quote needle) quoted)
            (notmuch-custom--remove-current-signature signature)))))))

(defun notmuch-custom-message-forward-subject (subject)
  "Return SUBJECT with all old markers replaced by one `Fwd:'."
  (notmuch-custom--canonical-subject subject 'forward))

(defun notmuch-custom--with-clean-forward-subject
    (original-function &rest arguments)
  "Call ORIGINAL-FUNCTION with canonical Notmuch forward subjects.
ARGUMENTS are passed unchanged to ORIGINAL-FUNCTION."
  (let ((message-make-forward-subject-function
         '(notmuch-custom-message-forward-subject)))
    (apply original-function arguments)))

(defun notmuch-custom--fill-message-region (start end &optional prefix)
  "Fill text between START and END to `message-fill-column'.
PREFIX is the citation prefix already present on every line, or nil for
ordinary forwarded text.  Do not alter a signature or an embedded forwarded
message, at any quote depth."
  (let* ((fill-column (or message-fill-column fill-column))
         (fill-prefix prefix)
         (case-fold-search t)
         (protected-start
          (save-excursion
            (goto-char start)
            (when (re-search-forward
                   (concat
                    "^[ \t]*\\(?:>[ \t]*\\)*"
                    "\\(?:"
                    "\\(?:--[ \t]*\\|_+\\)[ \t]*"
                    ;; Protect malformed legacy blocks too, where an earlier
                    ;; formatter joined `From:' onto the marker line.
                    "\\|" notmuch-custom--forward-start-regexp ".*"
                    "\\|" notmuch-custom--embedded-mail-header-regexp ".*"
                    "\\)$")
                   end t)
              (match-beginning 0))))
         (fill-end (or protected-start end)))
    (when (< start fill-end)
      (fill-individual-paragraphs start fill-end))))

(defun notmuch-custom-fill-cited-text ()
  "Fill the reply citation between point and mark to the message width.
This function follows `message-indent-citation' in
`message-indent-citation-function'."
  (when (and message-fill-column (mark t))
    (let ((start-marker (copy-marker (point)))
          (end-marker (copy-marker (mark t) t)))
      (unwind-protect
          (progn
            (notmuch-custom--fill-message-region
             start-marker end-marker message-yank-prefix)
            (goto-char start-marker)
            (set-mark (marker-position end-marker)))
        (set-marker start-marker nil)
        (set-marker end-marker nil)))))

(defun notmuch-custom--citation-zone-name (offset)
  "Return a readable UTC zone name for OFFSET seconds."
  (if (zerop offset)
      "UTC"
    (let* ((absolute (abs offset))
           (hours (/ absolute 3600))
           (minutes (/ (% absolute 3600) 60)))
      (format "UTC%c%d%s"
              (if (< offset 0) ?- ?+)
              hours
              (if (zerop minutes) "" (format ":%02d" minutes))))))

(defun notmuch-custom--citation-sender-name ()
  "Return the original sender's display name or email address."
  (let* ((from (mail-header-from message-reply-headers))
         (address-parts
          (and from (mail-extract-address-components from))))
    (or (car address-parts) (cadr address-parts) from "Unknown sender")))

(defun notmuch-custom-insert-citation-line ()
  "Insert a citation line with local time and optional sender-zone time."
  (let* ((date (mail-header-date message-reply-headers))
         (time (and date (ignore-errors (date-to-time date))))
         (sender-zone (and date (nth 8 (parse-time-string date))))
         (sender (notmuch-custom--citation-sender-name)))
    (if (not time)
        (insert (format "%s wrote:\n\n" sender))
      (let* ((local-date (decode-time time nil))
             (local-offset (car (current-time-zone time)))
             (different-zone
              (and (integerp sender-zone)
                   (/= sender-zone local-offset)))
             (sender-date
              (and different-zone (decode-time time sender-zone))))
        (insert
         (format "On %d/%d/%02d %02d:%02d %s%s, %s wrote:\n\n"
                 (nth 4 local-date)
                 (nth 3 local-date)
                 (% (nth 5 local-date) 100)
                 (nth 2 local-date)
                 (nth 1 local-date)
                 (notmuch-custom--citation-zone-name local-offset)
                 (if sender-date
                     (let ((same-day
                            (and (= (nth 3 local-date) (nth 3 sender-date))
                                 (= (nth 4 local-date) (nth 4 sender-date))
                                 (= (nth 5 local-date) (nth 5 sender-date)))))
                       (format " (%s%02d:%02d %s)"
                               (if same-day
                                   ""
                                 (format "%d/%d/%02d "
                                         (nth 4 sender-date)
                                         (nth 3 sender-date)
                                         (% (nth 5 sender-date) 100)))
                               (nth 2 sender-date)
                               (nth 1 sender-date)
                               (notmuch-custom--citation-zone-name
                                sender-zone)))
                   "")
                 sender))))))

(defun notmuch-custom-message-fill-setup ()
  "Fill newly inserted Notmuch reply citations to the message width."
  (setq-local message-indent-citation-function
              '(message-indent-citation notmuch-custom-fill-cited-text)))

(defun notmuch-custom--forward-message-id (forward-buffer)
  "Return the bare Message-ID from raw FORWARD-BUFFER."
  (let ((message-id
         (with-current-buffer forward-buffer
           (message-fetch-field "Message-ID"))))
    (unless message-id
      (error "Forwarded message has no Message-ID header"))
    (string-trim message-id "[<[:space:]]+" "[>[:space:]]+")))

(defun notmuch-custom--forward-original (forward-buffer)
  "Return Notmuch's decoded original-message object for FORWARD-BUFFER."
  (let* ((message-id (notmuch-custom--forward-message-id forward-buffer))
         (reply (notmuch-call-notmuch-sexp
                 "reply" "--format=sexp" "--format-version=5"
                 (notmuch-id-to-query message-id))))
    (or (plist-get reply :original)
        (error "Notmuch did not return the original message"))))

(defun notmuch-custom--clean-forward-url (url)
  "Unwrap known Scholar and Elsevier redirects in URL.
Only accept an explicit HTTP or HTTPS destination.  Do not fetch the URL
or remove parameters from the destination itself."
  (if (not (stringp url))
      url
    (require 'url-util)
    (save-match-data
      (let ((case-fold-search nil)
            destination)
        (cond
         ((string-match
               "\\`https?://scholar\\.google\\.com/scholar_\\(?:url\\|share\\)\\?\\(.*\\)\\'"
               url)
          (let ((query (match-string 1 url)))
            (when (string-match "\\(?:\\`\\|&\\)url=\\([^&]*\\)" query)
              (setq destination (url-unhex-string (match-string 1 query))))))
         ((string-match
           (concat "\\`https?://click\\.notification\\.elsevier\\.com/CL0/"
                   "\\(.+?\\)/[0-9]+/[^/]+/[^/]+\\'")
           url)
          (setq destination (url-unhex-string (match-string 1 url)))))
        (if (and destination
                 (string-match-p "\\`https?://[^[:space:]<>]+\\'" destination))
            destination
          url)))))

(defun notmuch-custom--clean-forward-text-urls ()
  "Unwrap known tracking URLs in rendered forwarding text."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "https?://\\(?:scholar\\.google\\.com/scholar_\\(?:url\\|share\\)\\?"
                    "\\|click\\.notification\\.elsevier\\.com/CL0/\\)"
                    "[^[:space:]<>]+")
            nil t)
      (let* ((url (match-string-no-properties 0))
             (clean (save-match-data (notmuch-custom--clean-forward-url url))))
        (unless (equal url clean)
          (replace-match clean t t))))))

(defun notmuch-custom--insert-rendered-link-destinations ()
  "Make SHR link destinations explicit before copying rendered plain text.
Keep a bare URL only once when it is already the visible link label."
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((start (point))
             (url (notmuch-custom--clean-forward-url
                   (get-text-property start 'shr-url)))
             (end (next-single-property-change
                   start 'shr-url nil (point-max))))
        (goto-char end)
        (when (and (stringp url)
                   (not (string-empty-p url))
                   (not (equal url
                               (notmuch-custom--clean-forward-url
                                (replace-regexp-in-string
                                 "[[:space:]]+" ""
                                 (buffer-substring-no-properties start end))))))
          (let ((begin (point)))
            (insert (concat " <" url ">"))
            ;; Do not inherit the link's properties onto the appended URL.
            (set-text-properties begin (point) nil)))))))

(defun notmuch-custom--render-forward-original (original)
  "Render decoded ORIGINAL using the same MIME selection as a reply."
  (with-temp-buffer
    (let ((notmuch-show-insert-text/plain-hook nil)
          (notmuch-show-max-text-part-size 0)
          (notmuch-show-insert-header-p-function
           notmuch-mua-reply-insert-header-p-function)
          (notmuch-show-process-crypto nil)
          (notmuch-show-indent-multipart nil)
          (mm-inline-override-types (notmuch--inline-override-types)))
      (cl-letf (((symbol-function 'notmuch-crypto-insert-sigstatus-button)
                 #'ignore)
                ((symbol-function 'notmuch-crypto-insert-encstatus-button)
                 #'ignore))
        (notmuch-show-insert-body original (plist-get original :body) 0)))
    (notmuch-custom--insert-rendered-link-destinations)
    (notmuch-custom--clean-forward-text-urls)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun notmuch-custom--insert-forward-headers (headers)
  "Insert the useful forwarding HEADERS into the current buffer."
  (dolist (spec '(("From" . :From)
                  ("To" . :To)
                  ("Cc" . :Cc)
                  ("Subject" . :Subject)
                  ("Date" . :Date)))
    (when-let* ((value (plist-get headers (cdr spec)))
                ((not (string-empty-p value))))
      (insert (car spec) ": " (notmuch-sanitize value) "\n")))
  (insert "\n"))

(defun notmuch-custom--insert-decoded-forward (original)
  "Insert decoded ORIGINAL as an inline forward in the current message.
The text is already MIME-decoded, so insert it directly instead of passing it
through `mime-to-mml' a second time."
  (if message-forward-before-signature
      (message-goto-body)
    (goto-char (point-max)))
  (insert
   "\n-------- Forwarded Message --------\n")
  (let ((forward-start (point))
        forward-end)
    (notmuch-custom--insert-forward-headers (plist-get original :headers))
    (let ((body-start (point)))
      (insert (notmuch-custom--render-forward-original original))
      (when message-fill-column
        (notmuch-custom--fill-message-region body-start (point))))
    (unless (bolp)
      (insert "\n"))
    (setq forward-end (point))
    (message-remove-ignored-headers forward-start forward-end))
  (message-position-point))

(defun notmuch-custom--format-native-forward-markers (original-function &rest args)
  "Format the outer markers inserted by ORIGINAL-FUNCTION with ARGS.
Apply only to inline Notmuch forwards, including attachment fallbacks."
  (if (not (derived-mode-p 'notmuch-message-mode))
      (apply original-function args)
    (let ((start (point))
          (size (buffer-size)))
      (prog1 (apply original-function args)
        (save-excursion
          (goto-char (+ start (- (buffer-size) size)))
          ;; Change only the newly inserted outer markers, preserving any
          ;; forwarded messages already quoted inside the original body.
          (when (and (bolp)
                     (looking-back
                      "-------------------- End of forwarded message --------------------\n"
                      start))
            (delete-region (match-beginning 0) (point)))
          (goto-char start)
          (when (looking-at
                 "\n-------------------- Start of forwarded message --------------------\n")
            (replace-match "\n-------- Forwarded Message --------\n" t t)))))))

(defun notmuch-custom--forward-has-attachments-p (parts)
  "Return non-nil if PARTS contain attachments or non-text MIME content.
Such messages need native MIME forwarding, not reply-style rendering."
  (cl-some
   (lambda (part)
     (let ((type (or (plist-get part :content-type) "")))
       (or (plist-get part :filename)
           (equal (plist-get part :content-disposition) "attachment")
           (if (string-prefix-p "multipart/" type)
               (notmuch-custom--forward-has-attachments-p
                (plist-get part :content))
             (not (member type '("text/plain" "text/html")))))))
   parts))

(defun notmuch-custom--message-forward-make-body-decoded
    (original-function forward-buffer &optional digest)
  "Use reply-style decoding only for text-only inline Notmuch forwards.
Preserve attachments through native MIME forwarding.
ORIGINAL-FUNCTION, FORWARD-BUFFER, and DIGEST are the arguments used by
`message-forward-make-body'."
  (if (or message-forward-as-mime
          digest
          (not (derived-mode-p 'notmuch-message-mode)))
      (funcall original-function forward-buffer digest)
    (condition-case error-data
        (let ((original (notmuch-custom--forward-original forward-buffer)))
          (if (notmuch-custom--forward-has-attachments-p
               (plist-get original :body))
              ;; Notmuch returns raw CRLF, sometimes with additional LF
              ;; headers.  Emacs's MIME parser expects LF separators.
              ;; Normalize a copy before parsing, not just the final draft.
              (let ((destination (current-buffer)))
                (with-temp-buffer
                  (insert-buffer-substring forward-buffer)
                  (notmuch-custom--normalize-crlf
                   (point-min) (point-max) 'keep)
                  (let ((normalized (current-buffer)))
                    (with-current-buffer destination
                      (funcall original-function normalized digest)))))
            (notmuch-custom--insert-decoded-forward original)))
      (error
       (message "Decoded Notmuch forward failed; using raw message: %s"
                (error-message-string error-data))
       (funcall original-function forward-buffer digest)))))

(defun notmuch-custom--normalize-inline-forward-crlf (&rest _ignored)
  "Remove carriage returns after composing an inline Notmuch forward.
The original message on disk is not changed.  MIME forwards are left byte-for-
byte intact because their embedded message may contain signed or binary data."
  (unless message-forward-as-mime
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t))
      (save-excursion
        (save-restriction
          (widen)
          ;; Header unfolding can leave a bare carriage return after the
          ;; newline has already been removed.  It is displayed as `^M', most
          ;; noticeably in the generated Subject header.
          (notmuch-custom--normalize-crlf
           (point-min) (point-max) 'remove)))
      (set-buffer-modified-p modified))))

;;; Thunderbird-compatible IMAP reply and forward flags

(defcustom notmuch-custom-imap-post-send-accounts nil
  "IMAP accounts used for server-side flag maintenance.
Flags are updated after sending; see
`notmuch-custom-imap-update-original-after-send'.  These accounts also
supply server stars through `notmuch-custom-sync-imap-starred'.
Each entry is a plist with these keys:

  :local-root       Thunderbird's local Maildir root for the account
  :host             IMAP server name
  :port             IMAP TLS port, normally 993
  :user             IMAP login name
  :auth-method      optional override: either `xoauth2' or `login'
  :auth-source-host optional host used to find the credential
  :auth-source-port optional port used to find the credential

When :auth-method is absent, use the auth-source entry's :auth value, falling
back to `login'.  The auth-source host and port overrides are useful when one
OAuth credential grants both SMTP and IMAP access but is stored under the SMTP
endpoint.  Message contents and credentials are never copied into this
variable."
  :type '(repeat sexp)
  :group 'notmuch-custom)

(defcustom notmuch-custom-imap-post-send-timeout 20
  "Maximum seconds to wait for each post-send IMAP operation."
  :type 'integer
  :group 'notmuch-custom)

(defcustom notmuch-custom-imap-post-send-enabled t
  "When non-nil, update the original message's IMAP flag after sending.
Replies receive `\\Answered'.  Forwards receive Thunderbird's `$Forwarded'
keyword when the server advertises support for it."
  :type 'boolean
  :group 'notmuch-custom)

(defun notmuch-custom--imap-account-for-file (file)
  "Return the configured IMAP account containing FILE."
  (let ((file (expand-file-name file)))
    (cl-find-if
     (lambda (account)
       (when-let* ((root (plist-get account :local-root)))
         (string-prefix-p (file-name-as-directory (expand-file-name root))
                          file)))
     notmuch-custom-imap-post-send-accounts)))

(defun notmuch-custom--imap-folder-for-file (file account)
  "Return FILE's IMAP mailbox name within ACCOUNT.
Thunderbird represents a nested local Maildir folder as `parent.sbd/child';
convert that representation back to the IMAP name `parent/child'."
  (let* ((root (file-name-as-directory
                (expand-file-name (plist-get account :local-root))))
         (relative (string-remove-prefix root (expand-file-name file))))
    (when (string-match
           "\\`\\(.+\\)/\\(?:cur\\|new\\)/[^/]+\\'" relative)
      (replace-regexp-in-string "\\.sbd/" "/" (match-string 1 relative)
                                t t))))

(defun notmuch-custom--imap-flag-for-tags (tags)
  "Return the Thunderbird-compatible IMAP flag implied by Notmuch TAGS."
  (cond
   ((or (equal tags notmuch-message-replied-tags)
        (member "+replied" tags))
    "\\Answered")
   ((or (equal tags notmuch-message-forwarded-tags)
        (member "+forwarded" tags))
    "$Forwarded")))

(defun notmuch-custom--imap-message-ids (query)
  "Return bare Notmuch message IDs matching QUERY."
  (mapcar (lambda (id) (string-remove-prefix "id:" id))
          (notmuch--process-lines notmuch-command
                                  "search" "--output=messages" query)))

(defun notmuch-custom--imap-jobs-for-change (query tags)
  "Return IMAP update jobs for Notmuch QUERY and queued TAGS."
  (when-let* ((flag (notmuch-custom--imap-flag-for-tags tags)))
    (cl-loop
     for message-id in (notmuch-custom--imap-message-ids query)
     append
     (cl-loop
      for file in (notmuch--process-lines
                   notmuch-command "search" "--output=files"
                   (notmuch-id-to-query message-id))
      for account = (notmuch-custom--imap-account-for-file file)
      for folder = (and account
                        (notmuch-custom--imap-folder-for-file file account))
      when folder
      collect (list :account account
                    :folder folder
                    :message-id message-id
                    :flag flag)))))

(defun notmuch-custom--imap-post-send-jobs ()
  "Build distinct IMAP jobs from this Notmuch composition buffer."
  (delete-dups
   (cl-loop for (query . tags) in notmuch-message-queued-tag-changes
            append (notmuch-custom--imap-jobs-for-change query tags))))

(defun notmuch-custom--imap-quote (string)
  "Return STRING as an IMAP quoted string."
  (concat "\""
          (string-replace "\"" "\\\""
                          (string-replace "\\" "\\\\" string))
          "\""))

(defun notmuch-custom--imap-wait-for (process regexp start description)
  "Wait for REGEXP after START in PROCESS's buffer.
Signal an error naming DESCRIPTION on failure or timeout."
  (let ((deadline (+ (float-time) notmuch-custom-imap-post-send-timeout))
        found)
    (while (and (process-live-p process)
                (< (float-time) deadline)
                (not found))
      (with-current-buffer (process-buffer process)
        (save-excursion
          (goto-char start)
          (setq found (re-search-forward regexp nil t))))
      (unless found
        (accept-process-output process 0.1)))
    (unless found
      (error "%s%s"
             description
             (if (process-live-p process) " timed out" " disconnected")))
    found))

(defun notmuch-custom--imap-command (process command description)
  "Send COMMAND through PROCESS and return its response.
DESCRIPTION identifies the operation in errors without exposing credentials."
  (let* ((number (1+ (or (process-get process 'notmuch-custom-imap-tag) 0)))
         (tag (format "NM%04d" number))
         (buffer (process-buffer process))
         start end status response)
    (process-put process 'notmuch-custom-imap-tag number)
    (with-current-buffer buffer
      (setq start (point-max)))
    (process-send-string process (format "%s %s\r\n" tag command))
    (let ((case-fold-search t))
      (notmuch-custom--imap-wait-for
       process
       (concat "^" (regexp-quote tag)
               "[ \t]+\\(OK\\|NO\\|BAD\\)\\(?:[ \t\r\n]\\|\\'\\)")
       start description)
      (with-current-buffer buffer
        (setq end (point-max)
              response (buffer-substring-no-properties start end))
        (when (string-match
               (concat "^" (regexp-quote tag)
                       "[ \t]+\\(OK\\|NO\\|BAD\\)")
               response)
          (setq status (upcase (match-string 1 response))))))
    (unless (equal status "OK")
      (error "%s failed (%s)" description (or status "invalid response")))
    response))

(defun notmuch-custom--imap-auth-entry (account)
  "Return ACCOUNT's matching entry from `auth-source'."
  (let* ((host (or (plist-get account :auth-source-host)
                   (plist-get account :host)))
         (port (or (plist-get account :auth-source-port)
                   (plist-get account :port)))
         (user (plist-get account :user))
         ;; JSON auth-source files preserve the JSON value's type, so accept
         ;; both a numeric port and its string representation.
         (entry
          (cl-loop for candidate-port in (delete-dups
                                          (list port (format "%s" port)))
                   thereis
                   (car (auth-source-search :max 1
                                            :host host
                                            :port candidate-port
                                            :user user
                                            :require '(:secret)
                                            :create nil)))))
    (unless entry
      (error "No auth-source credential for %s@%s:%s" user host port))
    entry))

(defun notmuch-custom--imap-authenticate (process account)
  "Authenticate PROCESS using ACCOUNT's auth-source credential."
  (let* ((user (plist-get account :user))
         (entry (notmuch-custom--imap-auth-entry account))
         (secret (or (auth-info-password entry)
                     (error "The auth-source entry for %s has no secret"
                            user)))
         (method
          (or (plist-get account :auth-method)
              (pcase (plist-get entry :auth)
                ((or 'xoauth2 "xoauth2") 'xoauth2)
                (_ 'login)))))
    (pcase method
      ('xoauth2
       (notmuch-custom--imap-command
        process
        (concat
         "AUTHENTICATE XOAUTH2 "
         (base64-encode-string
          (format "user=%s\1auth=Bearer %s\1\1" user secret) t))
        "IMAP OAuth authentication"))
      ('login
       (notmuch-custom--imap-command
        process
        (format "LOGIN %s %s"
                (notmuch-custom--imap-quote user)
                (notmuch-custom--imap-quote secret))
        "IMAP login"))
      (_
       (error "Unsupported IMAP authentication method: %S" method)))))

(defun notmuch-custom--imap-connect (account)
  "Open and authenticate a TLS IMAP connection for ACCOUNT."
  (let* ((host (plist-get account :host))
         (port (or (plist-get account :port) 993))
         (buffer (generate-new-buffer
                  (format " *notmuch-imap %s*" host)))
         process)
    (with-current-buffer buffer
      (set-buffer-multibyte nil)
      (buffer-disable-undo))
    (condition-case error-data
        (progn
          (setq process
                (with-timeout
                    (notmuch-custom-imap-post-send-timeout
                     (error "Connecting to IMAP %s:%s timed out" host port))
                  (open-network-stream
                   "notmuch-imap" buffer host port
                   :type 'tls :warn-unless-encrypted t)))
          (set-process-query-on-exit-flag process nil)
          (set-process-coding-system process 'binary 'binary)
          (let ((case-fold-search t))
            (notmuch-custom--imap-wait-for
             process "^\\* [ \t]*\\(?:OK\\|PREAUTH\\)\\(?:[ \t\r\n]\\|\\'\\)"
             (with-current-buffer buffer (point-min))
             "IMAP greeting"))
          (notmuch-custom--imap-authenticate process account)
          process)
      (error
       (when (processp process)
         (delete-process process))
       (when (buffer-live-p buffer)
         (kill-buffer buffer))
       (signal (car error-data) (cdr error-data))))))

(defun notmuch-custom--imap-flag-settable-p (select-response flag)
  "Return non-nil when SELECT-RESPONSE permits setting FLAG.
When the server omits PERMANENTFLAGS, let STORE provide the authoritative
answer instead."
  (let ((case-fold-search t))
    (if (string-match
         "\\[PERMANENTFLAGS[ \t]+(\\([^)]*\\))\\]" select-response)
        (let ((permanent-flags
               (split-string (match-string 1 select-response) nil t)))
          (or (member-ignore-case flag permanent-flags)
              (member "\\*" permanent-flags)))
      t)))

(defun notmuch-custom--imap-search-uids (process message-id)
  "Return UIDs for MESSAGE-ID in PROCESS's selected mailbox."
  (let* ((response
          (notmuch-custom--imap-command
           process
           (format "UID SEARCH HEADER Message-ID %s"
                   (notmuch-custom--imap-quote
                    (format "<%s>" message-id)))
           "IMAP Message-ID search"))
         uids)
    (dolist (line (split-string response "\r?\n" t))
      (when (string-match "\\`\\* SEARCH\\(?:[ \t]+\\(.*\\)\\)?\\'" line)
        (setq uids (split-string (or (match-string 1 line) "") nil t))))
    (cl-remove-if-not (lambda (uid) (string-match-p "\\`[0-9]+\\'" uid))
                      uids)))

(defun notmuch-custom--imap-sync-account (account jobs)
  "Apply JOBS through one authenticated connection for ACCOUNT.
Return the number of remote messages changed."
  (let ((process (notmuch-custom--imap-connect account))
        selected-folder select-response
        (changed 0))
    (unwind-protect
        (dolist (job jobs)
          (let ((folder (plist-get job :folder))
                (flag (plist-get job :flag)))
            (unless (equal folder selected-folder)
              (setq select-response
                    (notmuch-custom--imap-command
                     process
                     (format "SELECT %s"
                             (notmuch-custom--imap-quote
                              (utf7-encode folder t)))
                     (format "Selecting IMAP folder %s" folder))
                    selected-folder folder))
            (when (notmuch-custom--imap-flag-settable-p select-response flag)
              (when-let* ((uids
                           (notmuch-custom--imap-search-uids
                            process (plist-get job :message-id))))
                (notmuch-custom--imap-command
                 process
                 (format "UID STORE %s +FLAGS.SILENT (%s)"
                         (string-join uids ",") flag)
                 (format "Setting IMAP flag %s" flag))
                (cl-incf changed)))))
      (when (process-live-p process)
        (ignore-errors
          (notmuch-custom--imap-command process "LOGOUT" "IMAP logout"))
        (delete-process process))
      (when-let* ((buffer (and (processp process) (process-buffer process))))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    changed))

(defun notmuch-custom-imap-update-original-after-send ()
  "Update replied or forwarded status on the original IMAP message.
This function belongs on `message-sent-hook', which Emacs runs only after the
transport reports a successful send.  IMAP failures never turn a successful
mail submission into a send failure."
  (when (and notmuch-custom-imap-post-send-enabled
             (boundp 'notmuch-message-queued-tag-changes)
             notmuch-message-queued-tag-changes)
    (condition-case error-data
        (let ((jobs (notmuch-custom--imap-post-send-jobs))
              (changed 0))
          (dolist (account (delete-dups
                            (mapcar (lambda (job) (plist-get job :account))
                                    jobs)))
            (condition-case account-error
                (cl-incf changed
                         (notmuch-custom--imap-sync-account
                          account
                          (cl-remove-if-not
                           (lambda (job)
                             (equal account (plist-get job :account)))
                           jobs)))
              (error
               (message "Message sent, but IMAP status sync failed for %s: %s"
                        (plist-get account :host)
                        (error-message-string account-error)))))
          (when (> changed 0)
            (message "Message sent; updated Thunderbird IMAP status for %d source message%s"
                     changed (if (= changed 1) "" "s"))))
      (error
       (message "Message sent, but IMAP status sync could not be prepared: %s"
                (error-message-string error-data))))))

;;; IMAP starred synchronization

(defcustom notmuch-custom-imap-starred-sync t
  "When non-nil, mirror server stars during each refresh.
Use `notmuch-custom-imap-post-send-accounts' and scan the mailboxes
represented in the local Notmuch index.  Server stars use the separate
`imap-starred' tag; local `flagged' tags are never changed."
  :type 'boolean
  :group 'notmuch-custom)

(defun notmuch-custom--imap-starred-account-ids (account folders)
  "Read starred Message-IDs from ACCOUNT's FOLDERS without changing mail.
Signal an error on an incomplete scan so existing tags are preserved."
  (let ((process (notmuch-custom--imap-connect account))
        (case-fold-search t)
        ids)
    (unwind-protect
        (dolist (folder folders)
          (notmuch-custom--imap-command
           process
           (concat "EXAMINE " (notmuch-custom--imap-quote (utf7-encode folder t)))
           "Opening IMAP mailbox read-only")
          (let ((response (notmuch-custom--imap-command
                           process "UID SEARCH FLAGGED" "Searching IMAP stars")))
            (unless (string-match "^\\* SEARCH\\(?: +\\([0-9 ]*\\)\\)?\r?$" response)
              (error "Invalid IMAP starred search response"))
            (dolist (uid (split-string (or (match-string 1 response) "") " " t))
              (let* ((reply (notmuch-custom--imap-command
                             process
                             (format "UID FETCH %s (BODY.PEEK[HEADER.FIELDS (MESSAGE-ID)])"
                                     uid)
                             "Reading starred Message-ID"))
                     ;; Unfold header continuation lines before matching.
                     (header (replace-regexp-in-string "\r?\n[ \t]+" " " reply)))
                (unless (string-match "^Message-ID:[ \t]*<\\([^<>\r\n]+\\)>" header)
                  (error "Missing Message-ID for starred IMAP UID %s" uid))
                (push (match-string 1 header) ids)))))
      (when (process-live-p process)
        (ignore-errors
          (notmuch-custom--imap-command process "LOGOUT" "IMAP logout"))
        (delete-process process))
      (when-let* ((buffer (process-buffer process)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    ids))

(defun notmuch-custom-sync-imap-starred ()
  "Mirror server stars into Notmuch's `imap-starred' tag.
Scan indexed folders of `notmuch-custom-imap-post-send-accounts'.
Only update tags after every mailbox scan succeeds.  With no configured
accounts or indexed folders, leave existing tags alone.  The tag is
owned by this sync; ordinary `flagged' tags are left untouched."
  (interactive)
  (when notmuch-custom-imap-post-send-accounts
    (let ((folders (make-hash-table :test #'equal))
          remote-ids)
      (dolist (file (notmuch--process-lines
                     notmuch-command "search" "--output=files" "*"))
        (when-let* ((account (notmuch-custom--imap-account-for-file file))
                    (folder (notmuch-custom--imap-folder-for-file file account)))
          (cl-pushnew folder (gethash account folders) :test #'equal)))
      (when (> (hash-table-count folders) 0)
        ;; Finish all network reads before making any local tag changes.
        (maphash
         (lambda (account mailboxes)
           (setq remote-ids
                 (nconc (notmuch-custom--imap-starred-account-ids account mailboxes)
                        remote-ids)))
         folders)
        (let* ((local-ids (notmuch-custom--imap-message-ids "tag:imap-starred"))
               (remote-ids (delete-dups remote-ids))
               (add-ids (cl-set-difference remote-ids local-ids :test #'equal))
               (remove-ids (cl-set-difference local-ids remote-ids :test #'equal)))
          ;; Bound command sizes even for accounts with many stars.
          (dolist (change (list (cons "+imap-starred" add-ids)
                                (cons "-imap-starred" remove-ids)))
            (dolist (batch (seq-partition (cdr change) 100))
              (notmuch-tag (mapconcat #'notmuch-id-to-query batch " or ")
                           (list (car change)) 'omit)))
          (when (called-interactively-p 'any)
            (message "IMAP starred sync complete")))))))

;;; OAuth2 token-store migration

(defun notmuch-custom-reencrypt-oauth2-token-store ()
  "Re-encrypt `oauth2-token-file' to `plstore-encrypt-to'.
Create a mode-0600 backup of the original token store before replacing it.
This is intended for migrating an existing symmetric plstore after a GPG
recipient has been configured."
  (interactive)
  (require 'oauth2)
  (unless plstore-encrypt-to
    (user-error "Set plstore-encrypt-to before migrating the token store"))
  (let* ((file (expand-file-name oauth2-token-file))
         (backup (concat file ".symmetric-backup-"
                         (format-time-string "%Y%m%d-%H%M%S"))))
    (unless (file-exists-p file)
      (user-error "OAuth2 token store does not exist: %s" file))
    (copy-file file backup nil t)
    (set-file-modes backup #o600)
    (let ((store (plstore-open file)))
      (unwind-protect
          (progn
            ;; Force decryption so `plstore-save' encrypts the secret entries
            ;; again using the newly configured public-key recipient.
            (plstore--decrypt store)
            (plstore-save store)
            (set-file-modes file #o600))
        (plstore-close store)))
    (message "Re-encrypted %s; symmetric backup: %s" file backup)))

;;; Automatic refresh

(defcustom notmuch-custom-auto-refresh-interval 300
  "Seconds between automatic Notmuch indexing and refreshes."
  :type 'integer
  :group 'notmuch-custom)

(defvar notmuch-custom--refresh-timer nil
  "Timer used for automatic Notmuch indexing and refreshes.")

(defun notmuch-custom--inbox-message-ids ()
  "Return the message IDs currently in the inbox."
  (notmuch-call-notmuch-sexp
   "search" "--format=sexp" "--output=messages" "tag:inbox"))

(defun notmuch-custom--poll-with-senders (poll &rest args)
  "Call POLL with ARGS and announce senders of newly arrived inbox mail."
  (let ((before (condition-case nil
                    (let ((ids (make-hash-table :test #'equal)))
                      (dolist (id (notmuch-custom--inbox-message-ids))
                        (puthash id t ids))
                      ids)
                  (error nil))))
    (prog1 (apply poll args)
      (when before
        (condition-case error-data
            (let ((new-ids
                   (cl-remove-if
                    (lambda (id) (gethash id before))
                    (notmuch-custom--inbox-message-ids))))
              (when new-ids
                (let* ((addresses
                        (notmuch-call-notmuch-sexp
                         "address" "--format=sexp" "--output=sender"
                         "--deduplicate=address"
                         (mapconcat #'notmuch-id-to-query new-ids " or ")))
                       (senders
                        (mapcar
                         (lambda (address)
                           (let ((name (plist-get address :name)))
                             (replace-regexp-in-string
                              "[\n\r\t]+" " "
                              (if (and name (not (string-empty-p name)))
                                  name
                                (or (plist-get address :address)
                                    "Unknown sender")))))
                         addresses)))
                  (message "New mail from %s"
                           (if senders (string-join senders ", ")
                             "Unknown sender")))))
          (error
           (message "Mail fetched; could not read new senders: %s"
                    (error-message-string error-data))))))))

(defun notmuch-custom-poll-and-refresh ()
  "Index new mail and refresh all open Notmuch buffers.
Mirror IMAP stars first so the refreshed buffers display them."
  (interactive)
  (condition-case error-data
      (progn
        (notmuch-poll)
        (when notmuch-custom-imap-starred-sync
          (condition-case sync-error
              (notmuch-custom-sync-imap-starred)
            (error
             (message "IMAP starred sync failed: %s"
                      (error-message-string sync-error)))))
        (notmuch-refresh-all-buffers))
    (error
     (message "Automatic Notmuch refresh failed: %s"
              (error-message-string error-data)))))

(defun notmuch-custom-start-auto-refresh ()
  "Start automatic Notmuch indexing and refreshing."
  (interactive)
  (unless (and (numberp notmuch-custom-auto-refresh-interval)
               (> notmuch-custom-auto-refresh-interval 0))
    (user-error "Notmuch refresh interval must be positive"))
  (when (timerp notmuch-custom--refresh-timer)
    (cancel-timer notmuch-custom--refresh-timer))
  (setq notmuch-custom--refresh-timer
        (run-at-time 60
                     notmuch-custom-auto-refresh-interval
                     #'notmuch-custom-poll-and-refresh)))

;;; Setup

;;;###autoload
(defun notmuch-custom-setup ()
  "Enable the local Notmuch enhancements defined in this library."
  ;; Replace the former sender-specific advice if it is still loaded.
  (advice-remove 'notmuch-get-bodypart-text
                 'notmuch-custom--decode-pku-plain-text)
  (unless (advice-member-p #'notmuch-custom--decode-unlabeled-chinese-text
                           'notmuch-get-bodypart-text)
    (advice-add 'notmuch-get-bodypart-text :around
                #'notmuch-custom--decode-unlabeled-chinese-text))
  (unless (advice-member-p #'notmuch-custom--render-simple-html
                           'notmuch-show-insert-part-text/html)
    (advice-add 'notmuch-show-insert-part-text/html :around
                #'notmuch-custom--render-simple-html))
  (add-hook 'notmuch-show-insert-text/plain-hook
            #'notmuch-custom-decode-plain-text-entities)
  (add-hook 'notmuch-show-insert-text/plain-hook
            #'notmuch-custom-normalize-plain-text-newlines)
  (unless (advice-member-p #'notmuch-custom--poll-with-senders 'notmuch-poll)
    (advice-add 'notmuch-poll :around #'notmuch-custom--poll-with-senders))
  (unless (advice-member-p
           #'notmuch-custom--format-sent-folder-recipients
           'notmuch-tree-format-field)
    (advice-add 'notmuch-tree-format-field :around
                #'notmuch-custom--format-sent-folder-recipients))
  (unless (advice-member-p #'notmuch-custom--reflow-after-build
                           'notmuch-show--build-buffer)
    (advice-add 'notmuch-show--build-buffer :after
                #'notmuch-custom--reflow-after-build))
  (unless (advice-member-p #'notmuch-custom--insert-truncated-address-header
                           'notmuch-show-insert-header)
    (advice-add 'notmuch-show-insert-header :around
                #'notmuch-custom--insert-truncated-address-header))
  (dolist (function '(message-forward-make-body-plain
                      message-forward-make-body-digest-plain))
    (unless (advice-member-p #'notmuch-custom--format-native-forward-markers function)
      (advice-add function :around #'notmuch-custom--format-native-forward-markers)))
  (unless (advice-member-p #'notmuch-custom--normalize-inline-forward-crlf
                           'notmuch-mua-new-forward-messages)
    (advice-add 'notmuch-mua-new-forward-messages :after
                #'notmuch-custom--normalize-inline-forward-crlf))
  (unless (advice-member-p #'notmuch-custom--with-clean-forward-subject
                           'notmuch-mua-new-forward-messages)
    (advice-add 'notmuch-mua-new-forward-messages :around
                #'notmuch-custom--with-clean-forward-subject))
  (unless (advice-member-p #'notmuch-custom--normalize-current-reply-subject
                           'notmuch-mua-reply)
    (advice-add 'notmuch-mua-reply :after
                #'notmuch-custom--normalize-current-reply-subject))
  (unless (advice-member-p #'notmuch-custom--omit-signature-already-quoted
                           'notmuch-mua-reply)
    (advice-add 'notmuch-mua-reply :after
                #'notmuch-custom--omit-signature-already-quoted))
  (unless (advice-member-p
           #'notmuch-custom--message-forward-make-body-decoded
           'message-forward-make-body)
    (advice-add 'message-forward-make-body :around
                #'notmuch-custom--message-forward-make-body-decoded))
  (add-hook 'notmuch-message-mode-hook #'notmuch-custom-company-setup)
  (add-hook 'notmuch-message-mode-hook #'notmuch-custom-message-fill-setup)
  (add-hook 'message-sent-hook
            #'notmuch-custom-imap-update-original-after-send)
  (with-eval-after-load 'flycheck-languagetool
    (notmuch-custom--install-languagetool-filter))
  (with-eval-after-load 'flycheck-vale
    (notmuch-custom--install-vale-filter))
  ;; In a Notmuch composition buffer, killing should preserve the message as a
  ;; proper indexed draft instead of writing an unrelated *message* file.
  (define-key notmuch-message-mode-map (kbd "C-x k")
              #'notmuch-draft-postpone)
  (define-key notmuch-show-mode-map (kbd "W")
              #'notmuch-custom-reflow-current-message)
  (setq notmuch-show-part-button-default-action
        'notmuch-custom-open-part-with-default-application)
  (notmuch-custom-start-auto-refresh))

(provide 'notmuch-custom)

;;; notmuch-custom.el ends here
