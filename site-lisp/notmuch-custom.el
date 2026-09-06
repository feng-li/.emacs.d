;;; notmuch-custom.el --- Local enhancements for Notmuch -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Feng Li

;; Author: Feng Li <m@feng.li>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (company "1.0.0") (ivy "0.15.0") (notmuch "0.38"))
;; Keywords: mail, completion, multilingual

;;; Commentary:

;; Local Notmuch enhancements:
;;
;; - saved searches generated from the active Notmuch profile;
;; - address completion using literal text, full Pinyin, or Pinyin initials;
;; - compact, expandable To and Cc headers;
;; - prose checking limited to the subject and newly written message text;
;; - attachment opening through the desktop default application;
;; - display-only reflow of hard-wrapped plain-text messages;
;; - canonical reply and forward subject markers;
;; - CRLF normalization in inline forwarded messages; and
;; - coexistence of address completion in headers and word completion in bodies.

;;; Code:

(require 'cl-lib)
(require 'company)
(require 'ivy-pinyin-search)
(require 'mail-extr)
(require 'notmuch)
(require 'notmuch-address)
(require 'notmuch-company)
(require 'notmuch-show)
(require 'subr-x)

(defgroup notmuch-custom nil
  "Local enhancements for Notmuch."
  :group 'notmuch)

;;; Saved searches

(defun notmuch-custom-query-display-name (query-name)
  "Turn an ordered Notmuch QUERY-NAME into a display label.
The numeric prefix controls order, a hyphen becomes a space, and a
double hyphen becomes a folder separator."
  (let ((name (replace-regexp-in-string "\\`[0-9]+-" "" query-name)))
    (setq name (replace-regexp-in-string "--" " / " name))
    (replace-regexp-in-string "-" " " name)))

(defun notmuch-custom-saved-searches-from-profile ()
  "Build Emacs saved searches from the active profile's query.* entries."
  (let (query-names)
    (dolist (line (notmuch--process-lines notmuch-command "config" "list"))
      (when (string-match "\\`query\\.\\([^=]+\\)=" line)
        (push (match-string 1 line) query-names)))
    (mapcar (lambda (query-name)
              (list :name (notmuch-custom-query-display-name query-name)
                    :query (concat "query:" query-name)
                    :search-type 'unthreaded))
            (sort query-names #'string-lessp))))

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
   "\\|forwarded message\\|original message\\)"
   "[ \t]*-+[ \t]*"
   "\\|[ \t]*begin forwarded message:[ \t]*"
   "\\)")
  "Regexp matching a line that starts an inline forwarded message.")

(defconst notmuch-custom--forward-end-regexp
  "[ \t]*-+[ \t]*end of forwarded message[ \t]*-+[ \t]*"
  "Regexp matching a line that ends an inline forwarded message.")

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
                 (concat "\\`" notmuch-custom--forward-start-regexp "\\'")
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

(defun notmuch-custom-reflow-current-message ()
  "Reflow ordinary prose in the displayed message to the configured width.
Only inline text/plain parts are changed, and only in the Notmuch display
buffer.  Quoted, structured, signature, and forwarded text is left alone.
Refresh the buffer with `notmuch-show-refresh-view' to restore its rendering."
  (interactive)
  (unless (derived-mode-p 'notmuch-show-mode)
    (user-error "This command is only available while reading Notmuch mail"))
  (let* ((width (if (numberp notmuch-wash-wrap-lines-length)
                    notmuch-wash-wrap-lines-length
                  72))
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
    (unless blocks
      (user-error "The current message has no reflowable plain-text prose"))
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
    (message "Reflowed current message to %d columns; press g to restore"
             width)))

;;; Forwarded message cleanup

(defconst notmuch-custom--reply-subject-word-regexp
  "\\(?:re\\|reply\\|回复\\|回覆\\|答复\\|答覆\\|回信\\)"
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
ordinary forwarded text."
  (let ((fill-column (or message-fill-column fill-column))
        (fill-prefix prefix))
    (fill-individual-paragraphs start end)))

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
   "\n-------------------- Start of forwarded message --------------------\n")
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
    (insert
     "-------------------- End of forwarded message --------------------\n")
    (message-remove-ignored-headers forward-start forward-end))
  (message-position-point))

(defun notmuch-custom--message-forward-make-body-decoded
    (original-function forward-buffer &optional digest)
  "Use reply-style MIME decoding for an inline Notmuch forward.
ORIGINAL-FUNCTION, FORWARD-BUFFER, and DIGEST are the arguments used by
`message-forward-make-body'."
  (if (or message-forward-as-mime
          digest
          (not (derived-mode-p 'notmuch-message-mode)))
      (funcall original-function forward-buffer digest)
    (condition-case error-data
        (notmuch-custom--insert-decoded-forward
         (notmuch-custom--forward-original forward-buffer))
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
          (goto-char (point-min))
          (while (search-forward "\r\n" nil t)
            (replace-match "\n" t t))
          ;; Header unfolding can leave a bare carriage return after the
          ;; newline has already been removed.  It is displayed as `^M', most
          ;; noticeably in the generated Subject header.
          (goto-char (point-min))
          (while (search-forward "\r" nil t)
            (replace-match "" t t))))
      (set-buffer-modified-p modified))))

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

(defun notmuch-custom-poll-and-refresh ()
  "Index new mail and refresh all open Notmuch buffers."
  (interactive)
  (condition-case error-data
      (progn
        (notmuch-poll)
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
  (unless (advice-member-p #'notmuch-custom--insert-truncated-address-header
                           'notmuch-show-insert-header)
    (advice-add 'notmuch-show-insert-header :around
                #'notmuch-custom--insert-truncated-address-header))
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
  (unless (advice-member-p
           #'notmuch-custom--message-forward-make-body-decoded
           'message-forward-make-body)
    (advice-add 'message-forward-make-body :around
                #'notmuch-custom--message-forward-make-body-decoded))
  (add-hook 'notmuch-message-mode-hook #'notmuch-custom-company-setup)
  (add-hook 'notmuch-message-mode-hook #'notmuch-custom-message-fill-setup)
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
