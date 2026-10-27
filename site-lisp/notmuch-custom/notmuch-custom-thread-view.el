;;; notmuch-custom-thread-view.el --- Reverse thread trees and preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep folders flat, but open messages in a complete, split-preview thread.
;; Reverse the reply tree vertically, not the global chronology across branches.

;;; Code:

(require 'cl-lib)
(require 'notmuch)
(require 'notmuch-tree)

(declare-function notmuch-custom-extract-thread-attachments
                  "notmuch-custom-attachments" (directory))

(defgroup notmuch-custom-thread-view nil
  "Thread opening, ordering, and preview navigation."
  :group 'notmuch)

(defcustom notmuch-custom-tree-reverse-order t
  "Display replies above parents in threaded tree buffers.
This vertically mirrors the normal reply tree, preserving branch grouping
and indentation.  It is not a strict date sort across separate branches.
Flat folder lists are unaffected.  Set to nil and refresh to restore the
normal parent-first tree."
  :type 'boolean
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
Let Notmuch compute indentation and message metadata, then insert the rows
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
      (let ((first t))
        (cl-loop for (message next) on messages
                 do
                 ;; Compare with the row below, so the oldest message in
                 ;; each subject group shows the topic.  Notmuch saves this
                 ;; value on the row for later redraws after tag changes.
                 (let ((notmuch-tree-previous-subject
                        (when next
                          (notmuch-sanitize
                           (notmuch-show-strip-re
                            (plist-get (plist-get next :headers) :Subject))))))
                   (setq message (plist-put message :first first))
                   ;; Outline operates downwards. Group the entire reverse
                   ;; thread instead of folding descendants as parents.
                   (setq message (plist-put message :level (if first 1 2)))
                   (notmuch-tree-goto-and-insert-msg message))
                 (setq first nil))))))

(defun notmuch-custom-thread-view-setup ()
  "Install thread-opening keys and reverse-tree rendering idempotently."
  (setq notmuch-tree-show-out nil)
  (dolist (map (list notmuch-tree-mode-map notmuch-search-mode-map))
    (define-key map (kbd "RET") #'notmuch-custom-open-thread-tree)
    (define-key map [mouse-1] #'notmuch-custom-open-thread-tree))
  (define-key notmuch-tree-mode-map [down] #'notmuch-custom-tree-next-message)
  (define-key notmuch-tree-mode-map [up] #'notmuch-custom-tree-previous-message)
  (define-key notmuch-tree-mode-map (kbd "C-c C-e")
              #'notmuch-custom-extract-thread-attachments)
  (unless (advice-member-p #'notmuch-custom--tree-insert-reversed
                           'notmuch-tree-insert-forest-thread)
    (advice-add 'notmuch-tree-insert-forest-thread :around
                #'notmuch-custom--tree-insert-reversed)))

(provide 'notmuch-custom-thread-view)
;;; notmuch-custom-thread-view.el ends here
