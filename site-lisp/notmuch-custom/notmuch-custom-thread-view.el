;;; notmuch-custom-thread-view.el --- Reverse thread trees and preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep folders flat, but open messages in a complete, split-preview thread.
;; Reverse the reply tree vertically, not the global chronology across branches.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'notmuch)
(require 'notmuch-tree)

(declare-function notmuch-custom-extract-thread-attachments
                  "notmuch-custom-attachments" (directory))

(defgroup notmuch-custom-thread-view nil
  "Thread opening, ordering, and preview navigation."
  :group 'notmuch)

(defface notmuch-custom-key-hint
  '((((class color) (background dark))
     (:inherit default :background "#263445" :foreground "#dce6f1" :box nil))
    (((class color) (background light))
     (:inherit default :background "#e3eef8" :foreground "#23374d" :box nil))
    (t (:inherit default :inverse-video t :box nil)))
  "Face for the full navigation key-hint line in Notmuch reading views."
  :group 'notmuch-custom-thread-view)

(defcustom notmuch-custom-show-key-hints t
  "Show a compact navigation hint line in Notmuch reading views.
Use Customize to update existing views immediately.  Disabling this
option restores each buffer's previous header line."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'notmuch-custom--refresh-key-hints)
           (notmuch-custom--refresh-key-hints)))
  :group 'notmuch-custom-thread-view)

(defcustom notmuch-custom-tree-preview-layout 'side-by-side
  "Layout of the thread list and selected message preview.
`side-by-side' puts the thread list on the left and the message on the
right, separated by a vertical divider.  `stacked' keeps Notmuch's usual
thread list above the message.  If a window is too narrow to split side
by side, fall back to the stacked layout.
Changes apply the next time a message is previewed.  Flat folder lists
and whole-window message views are unaffected."
  :type '(choice (const :tag "Side by side (vertical divider)" side-by-side)
                 (const :tag "Stacked (horizontal divider)" stacked))
  :group 'notmuch-custom-thread-view)

(defcustom notmuch-custom-tree-reverse-order t
  "Display replies above parents in threaded tree buffers.
This vertically mirrors the normal reply tree, preserving branch grouping.
It is not a strict date sort across separate branches.
Flat folder lists are unaffected.  Set to nil and refresh to restore the
normal parent-first tree."
  :type 'boolean
  :group 'notmuch-custom-thread-view)

(defcustom notmuch-custom-tree-compact-markers t
  "Use aligned reply markers instead of nested branches in reverse trees.
Replies use \"› \" and original messages use \"• \", regardless of depth.
Reply grouping and order are preserved, but nesting depth is not displayed.
Set to nil and refresh to restore the full branch connectors."
  :type 'boolean
  :group 'notmuch-custom-thread-view)

(defcustom notmuch-custom-tree-subject-repeat-interval 4
  "Maximum spacing between full subjects in a reverse thread tree.
The top row and the bottom row of each subject group always show their
subjects.  With the default of 4, at most three abbreviated rows appear
between full subjects.  Set to 1 to show every subject, or nil to disable
periodic repetition while keeping the top row and subject-group bottoms."
  :type '(choice (const :tag "No periodic repetition" nil)
                 (integer :tag "Rows between full subjects"))
  :group 'notmuch-custom-thread-view)

(defun notmuch-custom-open-thread-tree (&optional arg)
  "Open the complete thread at point with a split message preview.
From a flat folder, retain the selected message as the preview target.
From search results, open only the selected thread, without search filters.
Inside an existing thread tree, preserve the usual RET behavior with ARG."
  (interactive "P")
  (cond
   ((and (derived-mode-p 'notmuch-tree-mode) (not notmuch-tree-unthreaded))
    (notmuch-tree-show-message arg))
   ((or (derived-mode-p 'notmuch-tree-mode)
        (derived-mode-p 'notmuch-search-mode))
    (let* ((target (when (derived-mode-p 'notmuch-tree-mode)
                     (notmuch-tree-get-message-id)))
           (query (if (derived-mode-p 'notmuch-tree-mode)
                      target
                    (notmuch-search-find-thread-id))))
      (unless query (user-error "No message or thread at point"))
      (notmuch-tree query nil target nil t nil (current-buffer)
                    notmuch-search-oldest-first nil)))
   (t (user-error "Open a Notmuch folder or search first"))))

(defun notmuch-custom--preview-tree-buffer ()
  "Return the thread tree owning the current message preview, if any."
  (let ((preview (current-buffer)))
    (cl-find-if
     (lambda (buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'notmuch-tree-mode)
              (not notmuch-tree-unthreaded)
              (eq notmuch-tree-message-buffer preview))))
     (buffer-list))))

(defun notmuch-custom-parent-view ()
  "Return to the parent folder or search, closing the opened thread.
From either pane of a thread, close both the tree and its preview in one
step.  Other Notmuch reading views retain the usual quit behavior."
  (interactive)
  (let* ((origin (current-buffer))
         (tree (if (derived-mode-p 'notmuch-tree-mode)
                   origin
                 (when (derived-mode-p 'notmuch-show-mode)
                   (notmuch-custom--preview-tree-buffer))))
         (parent (if tree
                     (buffer-local-value 'notmuch-tree-parent-buffer tree)
                   (and (derived-mode-p 'notmuch-show-mode)
                        notmuch-show-parent-buffer))))
    (if tree
        (with-current-buffer tree (notmuch-tree-quit t))
      (notmuch-bury-or-kill-this-buffer))
    (when (buffer-live-p parent)
      (switch-to-buffer parent))))

(defun notmuch-custom--tree-move-and-preview (count)
  "Move COUNT message rows and immediately preview in a real thread tree.
Keep ordinary cursor movement in flat folder lists.  Stop at the first or
last message rather than moving into the end-of-results footer."
  (if notmuch-tree-unthreaded
      (line-move count)
    (let ((before (notmuch-tree-get-message-id))
          (direction (if (< count 0) -1 1)))
      (catch 'boundary
        (dotimes (_ (abs count))
          (let ((start (point)))
            (forward-line direction)
            (unless (notmuch-tree-get-message-id)
              (goto-char start)
              (throw 'boundary nil)))))
      (when (and (notmuch-tree-get-message-id)
                 (or (not (equal before (notmuch-tree-get-message-id)))
                     (not (window-live-p notmuch-tree-message-window))))
        (notmuch-tree-show-message-in)))))

(defun notmuch-custom-tree-next-message (&optional count)
  "Move down COUNT rows, previewing the selected email within a thread."
  (interactive "p")
  (notmuch-custom--tree-move-and-preview (or count 1)))

(defun notmuch-custom-tree-previous-message (&optional count)
  "Move up COUNT rows, previewing the selected email within a thread."
  (interactive "p")
  (notmuch-custom--tree-move-and-preview (- (or count 1))))

(defun notmuch-custom--message-move (count)
  "Move COUNT message rows while keeping focus in the message pane.
Follow the owning thread's displayed order.  In a standalone show buffer,
use Notmuch's native within-buffer message navigation."
  (let* ((tree (notmuch-custom--preview-tree-buffer))
         (tree-window (and tree (get-buffer-window tree))))
    (if tree
        (progn
          (unless (window-live-p tree-window)
            (user-error "The thread list must be visible to navigate its preview"))
          ;; The old preview window and buffer may be deleted during this
          ;; call.  Select the replacement instead of restoring the old one.
          (select-window tree-window)
          (unwind-protect
              (notmuch-custom--tree-move-and-preview count)
            (when (window-live-p notmuch-tree-message-window)
              (select-window notmuch-tree-message-window))))
      (dotimes (_ (abs count))
        (if (< count 0)
            (notmuch-show-previous-message)
          (notmuch-show-next-message))))))

(defun notmuch-custom-message-next-message (&optional count)
  "Preview the next COUNT emails without leaving the message pane."
  (interactive "p")
  (notmuch-custom--message-move (or count 1)))

(defun notmuch-custom-message-previous-message (&optional count)
  "Preview the previous COUNT emails without leaving the message pane."
  (interactive "p")
  (notmuch-custom--message-move (- (or count 1))))

(defvar-local notmuch-custom--saved-header-line nil
  "Header line replaced by the navigation hints.")

(defvar-local notmuch-custom--key-hints-installed nil
  "Whether navigation hints replaced this buffer's header line.")

(defvar-local notmuch-custom--key-hint-face-cookie nil
  "Buffer-local face remapping for the navigation hint line.")

(defun notmuch-custom--key-hint-text ()
  "Return navigation hints for the current Notmuch view."
  (cond
   ((derived-mode-p 'notmuch-show-mode)
    " Alt+Up/Down: email   C-x o: pane   ?: help")
   ((derived-mode-p 'notmuch-tree-mode)
    (if notmuch-tree-unthreaded
        " RET: thread   Left: back   ?: help"
      " Up/Down: email   Left: back   RET: open   ?: help"))
   ((derived-mode-p 'notmuch-search-mode)
    " RET: thread   Left: back   ?: help")))

(defun notmuch-custom--install-key-hints ()
  "Apply the key-hint option to the current Notmuch reading buffer."
  (when (derived-mode-p 'notmuch-tree-mode 'notmuch-show-mode 'notmuch-search-mode)
    (if notmuch-custom-show-key-hints
        (progn
          (unless notmuch-custom--key-hints-installed
            (setq notmuch-custom--saved-header-line header-line-format
                  notmuch-custom--key-hints-installed t)
            (setq-local header-line-format '(:eval (notmuch-custom--key-hint-text))))
          (unless notmuch-custom--key-hint-face-cookie
            (setq notmuch-custom--key-hint-face-cookie
                  (face-remap-add-relative 'header-line 'notmuch-custom-key-hint))))
      (when notmuch-custom--key-hints-installed
        (setq-local header-line-format notmuch-custom--saved-header-line)
        (setq notmuch-custom--key-hints-installed nil))
      (when notmuch-custom--key-hint-face-cookie
        (face-remap-remove-relative notmuch-custom--key-hint-face-cookie)
        (setq notmuch-custom--key-hint-face-cookie nil)))))

(defun notmuch-custom--refresh-key-hints ()
  "Update key hints in existing Notmuch reading buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (notmuch-custom--install-key-hints)))
  (force-mode-line-update t))

(defun notmuch-custom--mirror-tree-symbol (string)
  "Vertically reflect box-drawing characters in tree symbol STRING.
Other characters, including custom ASCII symbols, are left alone."
  (apply #'string
         (mapcar (lambda (char)
                   (or (cdr (assq char '((?┬ . ?┴) (?┴ . ?┬)
                                         (?╰ . ?╭) (?╭ . ?╰)
                                         (?└ . ?┌) (?┌ . ?└)
                                         (?┘ . ?┐) (?┐ . ?┘)
                                         (?╯ . ?╮) (?╮ . ?╯))))
                       char))
                 string)))

(defun notmuch-custom--tree-insert-reversed (insert-thread thread)
  "Use INSERT-THREAD to render THREAD as a vertically reversed reply tree.
Let Notmuch compute tree structure and message metadata, then insert the rows
in reverse order through its normal target/preview-aware insertion path."
  (if (or notmuch-tree-unthreaded (not notmuch-custom-tree-reverse-order))
      (funcall insert-thread thread)
    (let ((notmuch-tree-thread-symbols
           (mapcar (lambda (pair)
                     (cons (car pair)
                           (notmuch-custom--mirror-tree-symbol (cdr pair))))
                   notmuch-tree-thread-symbols))
          messages)
      ;; Collect before inserting: reversing rendered text would lose target
      ;; selection, subject elision state, and asynchronous preview handling.
      (cl-letf (((symbol-function 'notmuch-tree-goto-and-insert-msg)
                 (lambda (message) (push message messages))))
        (funcall insert-thread (copy-tree thread)))
      (setq notmuch-tree-previous-subject nil)
      (let ((first t)
            (abbreviated-rows 0))
        (cl-loop for (message next) on messages
                 do
                 ;; Compare with the row below, so the oldest message in
                 ;; each subject group shows the topic.  Notmuch saves this
                 ;; value on the row for later redraws after tag changes.
                 (let* ((subject (notmuch-sanitize
                                  (notmuch-show-strip-re
                                   (plist-get (plist-get message :headers) :Subject))))
                        (next-subject
                         (when next
                           (notmuch-sanitize
                            (notmuch-show-strip-re
                             (plist-get (plist-get next :headers) :Subject)))))
                        (abbreviate
                         (and (not first) (equal subject next-subject)
                              (or (null notmuch-custom-tree-subject-repeat-interval)
                                  (< abbreviated-rows
                                     (1- notmuch-custom-tree-subject-repeat-interval)))))
                        (notmuch-tree-previous-subject (and abbreviate next-subject)))
                   (when notmuch-custom-tree-compact-markers
                     ;; Native tree status has two fragments at the root,
                     ;; plus one per reply level. Save the compact marker
                     ;; on the row so tag redraws retain the same layout.
                     (setq message
                           (plist-put message :tree-status
                                      (list (if (> (length (plist-get message :tree-status)) 2)
                                                "› "
                                              "• ")))))
                   (setq message (plist-put message :first first))
                   ;; Outline operates downwards. Group the entire reverse
                   ;; thread instead of folding descendants as parents.
                   (setq message (plist-put message :level (if first 1 2)))
                   (notmuch-tree-goto-and-insert-msg message)
                   (setq abbreviated-rows (if abbreviate (1+ abbreviated-rows) 0)))
                 (setq first nil))))))

(defun notmuch-custom--tree-preview-layout (show-message &rest args)
  "Call SHOW-MESSAGE with ARGS using the configured thread preview layout."
  (if (or notmuch-tree-unthreaded
          (not (eq notmuch-custom-tree-preview-layout 'side-by-side)))
      (apply show-message args)
    ;; Keep Notmuch's preview lifecycle, selection, and tagging behavior.
    ;; Redirect only its split operation while it creates the message pane.
    (let ((split-below (symbol-function 'split-window-vertically)))
      (cl-letf (((symbol-function 'split-window-vertically)
                 (lambda (&optional size)
                   (condition-case nil
                       (split-window-right)
                     (error (funcall split-below size))))))
        (apply show-message args)))))

(defun notmuch-custom-thread-view-setup ()
  "Install thread-opening keys and reverse-tree rendering idempotently."
  (setq notmuch-tree-show-out nil)
  (dolist (map (list notmuch-tree-mode-map notmuch-search-mode-map))
    (define-key map (kbd "RET") #'notmuch-custom-open-thread-tree)
    (define-key map [mouse-1] #'notmuch-custom-open-thread-tree))
  (dolist (map (list notmuch-tree-mode-map notmuch-search-mode-map))
    (define-key map [left] #'notmuch-custom-parent-view))
  (define-key notmuch-tree-mode-map [down] #'notmuch-custom-tree-next-message)
  (define-key notmuch-tree-mode-map [up] #'notmuch-custom-tree-previous-message)
  (define-key notmuch-show-mode-map (kbd "M-<down>")
              #'notmuch-custom-message-next-message)
  (define-key notmuch-show-mode-map (kbd "M-<up>")
              #'notmuch-custom-message-previous-message)
  (dolist (hook '(notmuch-tree-mode-hook notmuch-show-mode-hook
                  notmuch-search-mode-hook))
    (add-hook hook #'notmuch-custom--install-key-hints))
  (notmuch-custom--refresh-key-hints)
  (define-key notmuch-tree-mode-map (kbd "C-c C-e")
              #'notmuch-custom-extract-thread-attachments)
  (unless (advice-member-p #'notmuch-custom--tree-preview-layout
                           'notmuch-tree-show-message-in)
    (advice-add 'notmuch-tree-show-message-in :around
                #'notmuch-custom--tree-preview-layout))
  (unless (advice-member-p #'notmuch-custom--tree-insert-reversed
                           'notmuch-tree-insert-forest-thread)
    (advice-add 'notmuch-tree-insert-forest-thread :around
                #'notmuch-custom--tree-insert-reversed)))

(provide 'notmuch-custom-thread-view)
;;; notmuch-custom-thread-view.el ends here
