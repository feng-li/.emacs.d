;; init.el --- Feng Li's .emacs configurations  -*- lexical-binding: t; -*-
;;; Commentary:
;; Copyright: Feng Li <http://feng.li/>
;;
;; Download: https://github.com/feng-li/.emacs.d/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;      ("http" . "127.0.0.1:7890")
;;      ("https" . "127.0.0.1:7890")))

;; Add path for auto saved files
;;; Code:
(defvar my-base-save-list (concat (getenv "HOME") "/.config/emacs/"))
(defvar my-auto-save-list (concat my-base-save-list (system-name))) ;; host-specified
(unless (file-directory-p my-auto-save-list) (make-directory my-auto-save-list t))

(setq package-archives
      '(
        ;; ("melpa" . "https://melpa.org/packages/")
        ;; ("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa-cn"      . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ))

;; Local server socket dir. Some server does not allow to use the default
;; (setq server-use-tcp t)

;; Add personal load path recursively in front of the default load path if it exists.
(defvar my-site-lisp (concat user-emacs-directory "site-lisp"))
(if (file-directory-p my-site-lisp)
    (let ((default-directory my-site-lisp))
      (setq load-path
            (append
             (let ((load-path (copy-sequence load-path))) ;; Shadow
               (append
                (copy-sequence (normal-top-level-add-to-load-path '(".")))
                (normal-top-level-add-subdirs-to-load-path)))
             load-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(case-fold-search t)
 '(column-number-mode t)
 '(custom-safe-themes
   '("81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" default))
 '(doc-view-continuous t)
 '(global-font-lock-mode t nil (font-lock))
 '(highlight-doxygen-commend-start-regexp
   "\\(/\\*\\(!\\|\\*[^*]\\)\\|#\\('\\)\\|##\\('\\)\\|//\\(!\\|'\\|/[^/\12]\\)\\)")
 '(hl-paren-background-colors '("light gray" "steel blue" "lime green" "orange1"))
 '(indicate-empty-lines nil)
 '(neo-window-width 40)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(company-reftex wgrep pdf-tools proxy-mode gptel jinx envrc imenu-list lsp-latex lean-mode treesit-auto writegood-mode multiple-cursors pinyinlib company counsel swiper ivy ht flycheck-languagetool lsp-metals notmuch poly-R visual-fill-column keytar gnu-elpa-keyring-update use-package scala-mode lexic pandoc-mode synosaurus yaml-mode mw-thesaurus unfill powerthesaurus julia-mode neotree format-all adaptive-wrap highlight-doxygen electric-operator elpy markdown-mode dracula-theme yasnippet-snippets flycheck-julia math-symbol-lists polymode highlight-symbol popup iedit yasnippet magit ess dash auctex with-editor magit-popup))
 '(safe-local-variable-values '((TeX-engine . pdflatex)))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(send-mail-function 'mailclient-send-it)
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(warning-suppress-types '(((tar link)) (comp) (comp) (undo discard-info))))

;; Automatically install emacs packages
(unless (file-directory-p package-user-dir)
  (package-refresh-contents)
  (package-install-selected-packages)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Personal information
(setq frame-title-format "%b")
(setq user-full-name "Feng Li")
(setq user-mail-address "m@feng.li")
(setq gc-cons-threshold (* 1000 1024 1024)) ; 1000 MB

;; Auto-save list file prefix
(setq auto-save-list-file-prefix (concat my-auto-save-list "/.saves-"))

;; Term
(setenv "TERM" "xterm-256color")
(setenv "COLORTERM" "trueclor") ;; Ensure True Color in Systemd
(add-to-list 'term-file-aliases '("dumb" . "xterm-256color"))

;; Stop displaying strange symbols in place of the desired colored output
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; Allow shift-arrow keys and control-arrow keys under different tty
;; Set export TERM="xterm" in .bashrc and
;; term "xterm" in .screenrc.
;; (defadvice terminal-init-xterm (after select-shift-up activate)
;;   (define-key input-decode-map "\e[1;2A" [S-up]))

;; Theme
(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)

  ;; Modify background
  (unless (display-graphic-p)
    (set-face-background 'default "#1d1f21" nil)
    )
  )

;; (defun on-after-init ()
;;   (unless (display-graphic-p (selected-frame))
;;     (set-face-background 'default "#1d1f21" (selected-frame))))
;;     ;; (set-face-background 'default "#1d1f21" (selected-frame))))
;; (add-hook 'window-setup-hook #'on-after-init)
;; (setq dracula-use-24-bit-colors-on-256-colors-terms t)


;; The scratch settings
(setq initial-scratch-message nil) ;; Disable scratch information
(setq inhibit-startup-message t) ;;stop start up message
;; (setq fundamental-mode 'text-mode)
(setq initial-major-mode 'text-mode) ;; text mode in scratch
(setq major-mode 'text-mode)

;; Suspend and resume hook
(add-hook 'suspend-hook
          (function (lambda ()
                      (or (y-or-n-p
                           "Really suspend emacs? ")
                          (error "Suspend canceled")))))
(add-hook 'suspend-resume-hook
          (function (lambda () (message "Emacs resumed!"))))

;; TAB settings
(setq-default indent-tabs-mode nil)

;; SAVE MY PINK FINGER
;; Internal swap Ctrl and Alt key. Or use Gnome Tweak "Left Alt as Ctrl, Left Ctrl as Win, Left Win as Alt"
;; (setq x-ctrl-keysym 'alt)
;; (setq x-ctrl-keysym 'meta)
;; (setq x-alt-keysym 'ctrl)
;; (setq x-meta-keysym 'ctrl)

;; Switch to previous buffer with Shift-Tab
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "<backtab>") 'switch-to-previous-buffer)

;; Use y-n for short
(fset 'yes-or-no-p 'y-or-n-p)

;; Redraw-display when focused
(global-set-key (kbd "<f5>") 'redraw-display)

;; Set Fonts
(add-to-list 'default-frame-alist '(font . "M PLUS Code Latin 50-11")) ;

;; Menu bar
(menu-bar-mode t)

;; Tooltip mode
(tooltip-mode nil)

;; Global auto revert mode
;; (global-auto-revert-mode t)

;; Better vertical bar
(set-display-table-slot standard-display-table 'vertical-border ?│)
(set-face-background 'vertical-border (face-background 'mode-line))
(set-face-foreground 'vertical-border (face-background 'mode-line))

;; Remove weird ESC ESC key
;; (if (display-graphic-p)
;;     (progn
;;       ;; Do nothing with window system
;;       )
;;   (global-unset-key [?\e ?\e ?\e])
;;   (global-set-key [?\e ?\e escape] 'keyboard-escape-quit)
;;   )

(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Let Alt key be the meta key
(setq x-alt-keysym 'meta)

;; Mirror "C-x -> C-", and "M-x -> M-,"
(define-key global-map (kbd "C-,") ctl-x-map)
(define-key global-map (kbd "M-,") 'execute-extended-command)

;; Paste behavior
(define-key global-map (kbd "C-S-v") 'yank) ; Shift-Ctr-v to paste, same as terminal behavior
(define-key global-map (kbd "<mouse-3>") 'yank) ; Right click to paste

;; Global visual line mode with better indentation
;; (global-visual-line-mode t)
(setq-default fill-column 120)
(global-set-key (kbd "M-p") 'fill-paragraph) ;; mirror key for M-q
(dolist (hook '(message-mode-hook
                prog-mode-hook
                org-mode-hook
                mail-mode-hook
                text-mode-hook))
  (add-hook hook #'(lambda ()
                     (display-line-numbers-mode t)
                     (setq word-wrap-by-category t) ; Better CJK wrap support
                     )))

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-extra-text-width nil)
  (visual-fill-column-width (+ fill-column +6)) ;; fill-column+6

  :config
  (dolist (hook '(message-mode-hook
                  ;; prog-mode-hook
                  org-mode-hook
                  mail-mode-hook
                  text-mode-hook
                  reftex-toc-mode-hook))
    (add-hook hook #'(lambda ()
                     (visual-line-mode t)
                     (visual-fill-column-mode)
                     )))
  )

(use-package adaptive-wrap
  :ensure t
  :config
  (setq-default adaptive-wrap-extra-indent 0)
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
  )


;; shift selection
(setq shift-select-mode t)

;; allow mouse to select
;; (xterm-mouse-mode t)

;; make typing override text selection
(delete-selection-mode 1) ;

;; Cursor is bar: Not clear under console
(setq-default cursor-type 'box)
(setq-default visible-cursor t)
;;set visible-bell
(setq visible-bell t)

;; set big kill ring
(setq kill-ring-max 150)
(global-set-key (kbd "C-k") 'kill-whole-line)

;; auto-fill mode
(dolist (hook '(;;prog-mode-hook
                ;; text-mode-hook
                ;; LaTeX-mode-hook
                ;; markdown-mode-hook
                message-mode-hook
                org-mode-hook
	        mail-mode-hook
                ;; ess-mode-hook
                ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))


;; Unfilling a region joins all the lines in a paragraph into a single line for each
;; paragraphs in that region. It is the contrary of fill-region.
(use-package unfill
  :ensure t
  :config
  (define-key global-map (kbd "C-M-q") 'unfill-paragraph)
  )

;; https://www.reddit.com/r/emacs/comments/kwl0mc/lspdescribethingatpoint_config_improvement/
(add-to-list 'display-buffer-alist
             #'((lambda (buffer _) (with-current-buffer buffer
                                     (seq-some (lambda (mode)
                                                 (derived-mode-p mode))
                                               '(help-mode))))
                (display-buffer-reuse-window display-buffer-below-selected)
                (reusable-frames . visible)
                (window-height . 0.4)))

;; Security
(setq enable-dir-local-variables t) ;; Trust .dir-locals.el
(add-hook 'text-mode-hook
          ;; allow file-local variables in Emacs
          (lambda () (setq-local enable-local-variables :all)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copy with other applications
(setq select-enable-clipboard t)
(global-set-key (kbd "<mouse-2>") 'clipboard-yank)

;; Keybind to browse the kill ring
(global-set-key (kbd "C-c y")
                #'(lambda ()
                    (interactive)
                    (popup-menu 'yank-menu)))
(setq ring-bell-function (lambda ()  t))

;; Disable capitalize-key, conflict with tmux prefix ESC.
;; (global-unset-key (kbd "M-c"))
;; Kill the current buffer, without confirmation.
;; Kill buffer without conformation
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Bind undo with the common keyboard
(global-set-key (kbd "C-z") 'undo)

;; Unset suspend-frame key
(global-unset-key (kbd "C-x C-z"))

;; Personal global key settings
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<select>") 'end-of-buffer)
(global-set-key (kbd "<f9> n") 'new-frame)
(global-set-key (kbd "<f9> g") 'rgrep)
(global-set-key (kbd "<f9> f") 'find-name-dired)
(global-set-key (kbd "<f9> q") 'fill-region-as-paragraph)
(global-set-key (kbd "<f9> TAB") 'indent-relative)
(global-set-key (kbd "<f2>") 'next-multiframe-window) ;; Circulate among windows
(global-set-key (kbd "C-x o") 'next-window-any-frame) ;; Circulate among windows
;; Follow mode (dual pages display)
(global-set-key (kbd "C-<f2>")  'follow-delete-other-windows-and-split)

;; Control-tab to switch among buffers
;; (global-set-key (kbd "<backtab>") 'next-buffer)

;; Unset 'exchange-point-and-mark' key which may cause conflicts with Fcitx
(global-unset-key (kbd "C-x C-x"))

;; Key bind to increase and decrease text size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; (global-set-key (kbd "M-SPC") 'set-mark-command) ;It was C-SPC

;; Insert current time, Linux only?
(global-set-key (kbd "C-c t") 'my-insert-time)
(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y")))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Desktop and history
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable backup files (*~)
(setq make-backup-files nil)

;; Desktop save mode
(defvar my-desktop-path my-auto-save-list)
(setq desktop-path (list my-desktop-path))
(setq desktop-dirname my-desktop-path)

(desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq desktop-restore-frames nil)

;; save mini buffer history
(savehist-mode 1)
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(setq savehist-file (concat my-desktop-path "/savehist-file.el"))

;; Add a hook when emacs is closed to we reset the desktop modification time (in this way
;; the user does not get a warning message about desktop modifications)
(add-hook 'kill-emacs-hook
          (lambda ()
            ;; Reset desktop modification time so the user is not bothered
            (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))))

(setq desktop-buffers-not-to-save
      (concat "\\("
              "^\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\*Help*\\|\\.gz$"
              "\\)$"))
(setq desktop-globals-to-save
      (quote
       (desktop-missing-file-warning
        search-ring
        regexp-search-ring
        register-alist
        file-name-history)))
(add-to-list 'desktop-modes-not-to-save '(dired-mode Info-mode fundamental-mode))

;; save-place-mode
(setq save-place-file (concat my-auto-save-list "/save-place-file.el"))

;; bookmarks
;; every time bookmark is changed, automatically save it
(setq bookmark-save-flag 1)
(setq bookmark-default-file (concat my-auto-save-list "/bookmarks"))
(global-set-key (kbd "<f9> b") 'bookmark-bmenu-list)
(defun bookmark-current-file ()
  (interactive)
  (bookmark-set (buffer-file-name) nil))
(global-set-key (kbd "<f9> m") 'bookmark-current-file)


;;Session (keep sections with different machines)
;; (use-package session"
;;   '(progn
;;      (setq session-use-package nil)
;;      (add-hook 'after-init-hook 'session-initialize)
;;      ;; Save sessions with customization
;;      (setq session-save-file (concat my-auto-save-list "/session-save-file.el"))
;;      ))

;; Eshell
(setq eshell-directory-name (concat my-auto-save-list "/eshell"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide and Show code blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (hook '(after-text-mode-hook c-mode-hook org-mode-hook python-mode-hook mail-mode-hook ess-mode-hook))
  (add-hook hook #'(lambda () (hs-minor-mode))))
(global-set-key (kbd "M-+") 'hs-toggle-hiding)
(global-set-key (kbd "M-*") 'hs-show-all)


;; Electric operators
(use-package electric-operator
  :ensure t
  :config
  (electric-pair-mode t)

  (dolist (hook
           '(python-mode-hook
             python-ts-mode
             c-mode-hook
             c++-mode-hook
             LaTeX-mode-hook
             ess-r-mode-hook))
    (add-hook hook #'(lambda () (electric-operator-mode 1))))

  ;; Customize rules
  (electric-operator-add-rules-for-mode 'python-mode (cons "*" nil))
  (electric-operator-add-rules-for-mode 'python-mode (cons "/" nil))
  (electric-operator-add-rules-for-mode 'python-mode (cons "?" nil))

  ;; Add support for various treesitter based modes
  (apply #'electric-operator-add-rules-for-mode 'inferior-python-mode (electric-operator-get-rules-for-mode 'python-mode))
  (apply #'electric-operator-add-rules-for-mode 'python-ts-mode (electric-operator-get-rules-for-mode 'python-mode))

  (setq electric-operator-R-named-argument-style "spaced")
  )

;; Goto matched parenthesis
(global-set-key (kbd "?") 'goto-match-paren) ;;
(defun goto-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t (self-insert-command (or arg 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab, Octave, yaml mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let m-file connected with octave mode.
(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))

(use-package yaml-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General IDE settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package counsel
  :ensure t
  :custom
  (counsel-rg-base-command
   '("rg" "--max-columns" "1600" "--with-filename" "--no-heading" "--line-number" "--color" "never" "%s"))

  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)

  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-,") 'counsel-M-x) ; mirror of M-x
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c G") 'counsel-git-grep)
  (global-set-key (kbd "C-c g") 'counsel-ag) ;; find patten within git repository
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map (kbd "C-c C-v") 'ivy-occur)

  (defun my-counsel-rg-in-dir ()
    "Run `counsel-rg` in a directory of your choice."
    (interactive)
    (let ((dir (read-directory-name "Search in directory: ")))
      (counsel-rg nil dir)))
  (global-set-key (kbd "C-c r") #'my-counsel-rg-in-dir)

  ;; Use C-j for immediate termination with the current value, and RET for continuing
  ;; completion for that directory. This is the ido behaviour.
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

  ;; Ignore hidden files.
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  )

;; let `ivy-read' support Chinese pinyin, toggle with `
(use-package pinyinlib
  :ensure t
  :config
  (defvar-local my-pinyin-search-prefix "`")
  (defun re-builder-pinyin (str)
    (or (pinyin-to-utf8 str)
        (ivy--regex-plus str)
        (ivy--regex-ignore-order)
        ))
  (setq ivy-re-builders-alist
        '(
          (t . re-builder-pinyin)
          ))
  (defun my-pinyinlib-build-regexp-string (str)
    (progn
      (cond ((equal str ".*")
             ".*")
            (t
             (pinyinlib-build-regexp-string str t))))
    )
  (defun my-pinyin-regexp-helper (str)
    (cond ((equal str " ")
           ".*")
          ((equal str "")
           nil)
          (t
           str)))
  (defun pinyin-to-utf8 (str)
    (cond ((equal 0 (length str))
           nil)
          ((equal (substring str 0 1) my-pinyin-search-prefix)
           (mapconcat 'my-pinyinlib-build-regexp-string
                      (remove nil (mapcar 'my-pinyin-regexp-helper
                                          (split-string (replace-regexp-in-string my-pinyin-search-prefix "" str) "")))
                      ""))
          nil))
)

;; iedit mode
(use-package iedit
  :ensure t
  :config
  (global-set-key (kbd "C-c i") 'iedit-mode)
  )

(use-package multiple-cursors
  :ensure t

  :config
  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  )


;; Ibuffer mode
(use-package ibuffer
  :ensure t
  :config
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "<f9> i") 'ibuffer)

  (setq dired-omit-mode t)
  (add-hook 'dired-mode-hook
            (lambda ()
              (setq dired-omit-files "^\\.[a-z|A-Z]+\\|^\\.?#\\|^\\.$")
              (setq dired-omit-extensions
                    '(".pyc" "CVS/" "~" ".o" ".bin" ".bak" ".obj" ".map" ".a"
                      ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot"
                      ".dvi" ".fmt" ".tfm" ".class" ".fas" ".lib" ".x86f"
                      ".sparcf" ".lo" ".la" ".toc" ".aux" ".cp" ".fn" ".ky"
                      ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps"
                      ".vrs" ".idx" ".lof" ".lot" ".glo" ".blg" ".cp" ".cps"
                      ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps"
                      ".vr" ".vrs" ".Rc" ))
              (setq dired-listing-switches "-hla")
              (define-key dired-mode-map (kbd "<return>")
                'dired-find-alternate-file) ; was dired-advertised-find-file
              (define-key dired-mode-map (kbd "<delete>") 'dired-do-delete)
              (define-key dired-mode-map (kbd "<f9> DEL")
                (lambda () (interactive) (find-alternate-file "..")))
              (dired-omit-mode 1)
              (local-set-key (kbd "<f9> h") 'dired-omit-mode)))
  (put 'dired-find-alternate-file 'disabled nil)


  (setq ibuffer-saved-filter-groups
        (quote
         (("default"
           ("Scripts" (or
                       (mode . ess-mode)
                       (mode . prog-mode)
                       (mode . shell-script-mode)
                       (mode . sh-mode)
                       (mode . python-mode)
                       (mode . makefile-mode)
                       (mode . snippet-mode)
                       (mode . emacs-lisp-mode)))
           ("Files" (or
                     (mode . LaTeX-mode)
                     (mode . latex-mode)
                     (mode . markdown-mode)
                     (mode . bibtex-mode)
                     (mode . org-mode)))
           ("Config" (or
                      (mode . yaml-mode)
                      (mode . conf-unix-mode)))

           ("Proc" (or
                    (mode . inferior-ess-mode)))
           ("Help" (or
                    (mode . help-mode)
                    (mode . lexic-mode)
                    (mode . ess-help-mode)
                    (mode . Info-mode)))
           ("Messages" (or
                        (name . "^\\*scratch\\*$")
                        (name . "^\\*Bookmark List\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Completions\\*$")
                        (mode . fundamental-mode)))
           ))))
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default"))))

;; Ido mode
(use-package ido
  :ensure t
  :config
  (ido-mode t)
  (setq ido-use-virtual-buffers nil)
  (setq ido-enable-flex-matching nil)
  (setq ido-ignore-extensions t)
  (setq ido-save-directory-list-file (concat my-auto-save-list "/ido-save-directory-list-file.el"))
  (setq ido-ignore-files ; this also works with directories with c-x c-f
        '("\\.Rc$" "\\.dvi$" "\\.pdf$" "\\.ps$" "\\.out$" "\\.fls$" "\\.spl$"
          "\\.fff$" "\\.ttt$" "\\.log$" "\\.ods$" "\\.eps$" "\\#$" "\\.png$" "\\~$"
          "\\.RData$" "\\.nav$" "\\.snm$" "\\`\\.\\./" "\\`\\./" "\\.synctex.gz$"
          "\\.fdb_latexmk$" "\\.tar.gz$" "\\.zip$" "\\.o$" "\\.tar$" "\\.Rproj$"
          "\\.Rcheck$" "\\.doc$" "\\.docx$" "\\.Rhistory$" "auto/" "__pycache__/"
          "\\.bcf$" "\\.run.xml$" "_region_.tex$" "\\.xdv$" "\\.DS_Store$"
          "\\.cfg$" "\\.bak$" "\\.gitignore" "\\.tmp$"))

  (setq  ido-ignore-directories ; only works with ido-dired
         '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`.git/" "\\`.ropeproject/" "\\`\\.\\./"
           "\\`\\./" "\\`_bookdown_files/" "__pycache__/"))

  (setq ido-ignore-buffers
        '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
          "*scratch*" "^\\*Ibuffer*" "^\\*ESS-errors*" "^\\*Warnings*" "*TeX Help*"
          "*Pymacs*" "*Flymake log*" "\\.log$" "^\\*.*Completions\\*$" "^\\*Ediff"
          "^\\*tramp" "^\\*cvs-" "_region_" "^TAGS$" "^\\*Ido"
          "^\\*.*dictem buffer\\*$" "^\\*inferior-lisp*" "^\\*Compile-Log\\*"
          "*output*" "\.\*output*" "^\\*EGLOT*" "^\\*Async-native-compile-log*"
          "^\\*lsp-log*" "^\\*grammarly-ls::stderr*" "^\\*grammarly-ls*"))

  (defun ido-kill-emacs-hook () (ignore-errors (ido-save-history)))
  )

;; ElDoc mode
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook #'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook #'turn-on-eldoc-mode)

;; Comint for input history and  scrolling
(use-package comint
  :ensure nil
  :config
  (define-key comint-mode-map (kbd "C-<up>")'comint-previous-matching-input-from-input)
  (define-key comint-mode-map (kbd "C-<down>") 'comint-next-matching-input-from-input)
  (define-key comint-mode-map (kbd "C-k") 'comint-kill-input)

  ;; Comint history length
  (setq comint-input-ring-size 5000)
  (setq comint-read-input-ring t)

  ;; Comint scroll output
  (setq comint-scroll-to-bottom-on-output 'others)
  (setq comint-scroll-show-maximum-output t)
  (setq comint-scroll-to-bottom-on-input 'this)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-move-point-for-output t)

  ;; Clear buffer output
  ;; Emacs 25's new default function was bound to (C-C, C-O)
  (define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto completion settings (company mode, yasnippet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure t
  :config

  ;; Make yasnippet treat LaTeX-mode as latex-mode
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'latex-mode)))

  (yas-global-mode 1)
  ;; (setq yas-snippet-dirs
  ;;       '(;; personal snippets
  ;;         "~/.emacs.d/snippets"
  ;;         ;; snippet collection
  ;;         ;; "~/.emacs.d/site-lisp/yasnippet-snippets/snippets"
  ;;         ))
  )

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-company-mode)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay
        (lambda () (if (company-in-string-or-comment) nil 0.5)))

  (setq company-files-exclusions '(".git/" ".DS_Store" "auto/"))

  ;; Preserve initial cases
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-other-buffers t)

  ;; Add yasnippet support for all company backends
  (add-to-list 'company-backends '(company-capf :with company-yasnippet))

  (defun my-text-mode-hook ()
    (setq-local company-backends
                '((company-yasnippet
                   company-dabbrev
                   company-ispell :separate)
                  company-files)))
  (add-hook 'text-mode-hook #'my-text-mode-hook)

  ;; https://github.com/abo-abo/oremacs/issues/38#issuecomment-948472184
  (setq company-show-quick-access t)
  (defun hz-company-complete-number ()
    "Convert the company-quick-access-keys to the candidates' row NUMBER visible on the
  tooltip, and then feed it to `company-complete-number' to quickly select and insert
  company candidates.  If the currently entered character is belongs to
  company-quick-access-keys and a part of the candidate simultaneously, append it to the
  currently entered string to construct new company-prefix."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))

      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
	  (self-insert-command 1)
	(company-complete-tooltip-row
	 (cond
	  ((equal k "1") 1)
	  ((equal k "2") 2)
	  ((equal k "3") 3)
	  ((equal k "4") 4)
	  ((equal k "5") 5)
	  ((equal k "6") 6)
	  ((equal k "7") 7)
	  ((equal k "8") 8)
	  ((equal k "9") 9)
	  ((equal k "0") 10)
	  )
	 ))))
  (let ((c-a-map company-active-map)
	(c-s-map company-search-map))
    (mapc (lambda (x)
	    (define-key c-a-map (format "%s" x) #'hz-company-complete-number))
	  '(1 2 3 4 5 6 7 8 9 0))
    (mapc (lambda (x)
	    (define-key c-s-map (format "%s" x) #'hz-company-complete-number))
	  '(1 2 3 4 5 6 7 8 9 0)))

  )


;; Font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t) ;; Highlight parentheses

(use-package highlight-parentheses
  :ensure t
  :disabled t
  :config
  (define-globalized-minor-mode global-highlight-parentheses-mode
    highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
  (global-highlight-parentheses-mode t)) ;; Highlight symbols

;; parentheses mode
(show-paren-mode t)
;; (setq show-paren-style 'expression) ;; highlight whole block

;; Commenting
(global-set-key (kbd "M-3") 'comment-or-uncomment-region)
(global-set-key (kbd "M-0") 'comment-or-uncomment-region) ;; mirror

;; Add extra info path
(use-package info-look
  :ensure t
  :config
  (add-to-list
   'Info-default-directory-list (concat user-emacs-directory "info")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("neomutt" . message-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control and Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ediff
(setq ediff-ignore-similar-regions t)

(setq vc-handled-backends ()) ;; Disable vc-git and use magit
(vc-mode -1)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               (concat user-emacs-directory "/magit/Documentation/"))
  )
(setq vc-follow-symlinks nil)

(use-package magit
  :ensure t
  :defer nil
  :config
  ;; Save transient file with customization
  (setq transient-history-file (concat my-auto-save-list "/transient-history-file.el"))

  ;; magit with-editor support
  (define-key (current-global-map)
    [remap async-shell-command] 'with-editor-async-shell-command)
  (define-key (current-global-map)
    [remap shell-command] 'with-editor-shell-command)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling checking & dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spelling Check
(use-package ispell
  :ensure t
  :config
  ;; (setq ispell-dictionary "american")

  ;; Hunspell 1.7 is broken with emacs 26.1
  (defun ispell-get-coding-system () 'utf-8)
  (setq ispell-use-framepop-p t)

  ;; Prefer hunspell if exits
  (if (locate-file "hunspell" exec-path)
      (progn
        (setq ispell-program-name "hunspell")
        (setq ispell-really-hunspell t)
        (setenv "DICPATH" (concat user-emacs-directory "dicts/hunspell"))
        (setq ispell-local-dictionary "en_US")
        (setq ispell-local-dictionary-alist
              '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
                 ("-d" "en_US,en_GB") nil UTF-8)))
        )
    )

  (global-set-key (kbd "<f9> 4") 'ispell-word))

;; jinx directly calling the widely-used API of the Enchant library.
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-4" . jinx-correct)
	 ("C-M-$" . jinx-languages))

  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")) ;; Disable Chinese check

  )

(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil)
  )


;; FlyCheck
;; Enable nice rendering of diagnostics like compile errors.

;; Remove flymake mode and using flycheck mode
(remove-hook 'prog-mode-hook #'flymake-mode)
(remove-hook 'text-mode-hook #'flymake-mode)
(remove-hook 'LaTeX-mode-hook #'flymake-mode)

(use-package flycheck
  :ensure t
  :custom
  (flycheck-checker-error-threshold 800)
  (flycheck-check-syntax-automatically (quote (idle-change mode-enabled))) ; save
  (flycheck-idle-change-delay 3) ;; Set delay based on what suits you the best
  (global-flycheck-mode t)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-flake8rc '(".flake8" "setup.cfg" "tox.ini"
                       "~/.config/flake8/setup.cfg"
                       "~/.config/flake8/tox.ini"))

  :config
  ;; Checkers for Python
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-python-pylint-executable "pylint")
  ;; make python-pylint run after python-flake8

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language and writing https://languagetool.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LanguageTool https://languagetool.org/download/
(use-package flycheck-languagetool
  :custom
  (flycheck-languagetool-active-modes
   '(latex-mode LaTeX-mode org-mode markdown-mode gfm-mode))
  :config
  ;; (setq flycheck-languagetool-active-modes '(LaTeX-mode org-mode markdown-mode))
  (setq flycheck-languagetool-url  "http://localhost:8081")
  (flycheck-languagetool-setup)
  )


;; (use-package flycheck-grammarly
;;   :defer nil
;;   :custom
;;   (flycheck-grammarly-active-modes
;;    '(LaTeX-mode org-mode markdown-mode gfm-mode))
;;   :config
;;   (setq flycheck-grammarly-check-time 0.8)

;;   (flycheck-grammarly-setup)
;;   )

(use-package synosaurus
  :config
  (dolist (hook '(text-mode-hook latex-mode-hook LaTeX-mode-hook prog-mode-hook org-mode-hook markdown-mode-hook))
    (add-hook hook (lambda () (synosaurus-mode))))
  (setq synosaurus-choose-method 'popup) ; popup, ido
  ;; (setq synosaurus-backend  'Wordnet) ; apt install wordnet
  (define-key synosaurus-mode-map (kbd "<f9> s") 'synosaurus-choose-and-replace)
  )


;; Fly spell performance
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

;; Fly spell mode for major mode, use jinx now.
;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
;; (dolist (hook '(text-mode-hook LaTeX-mode-hook markdown-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;; (add-hook 'LaTeX-mode-hook (function (lambda () (setq ispell-parser 'tex))))

;; Disable flyspell for special modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Auto correct spelling mistakes
(global-set-key (kbd "<f9> c") 'flyspell-auto-correct-word)

;; StarDict
;; apt install sdcv
;; https://github.com/Dushistov/sdcv
(use-package lexic
  :defer nil
  :config
  (setq lexic-dictionary-path (concat user-emacs-directory "dict/sdcv/"))
  (setq lexic-dictionary-list
        '(;; "Soule's Dictionary of English Synonyms (En-En)"
          "Merriam-Webster's Collegiate Thesaurus (En-En)"
          "Merriam-Webster's Advanced Learner's Dictionary (En-En)"
          "Longman Dictionary of Common Errors (En-En)"))
  (global-set-key (kbd "<f9> d") 'lexic-search)
  )

(use-package mw-thesaurus
  :defer nil
  :config
  (setq mw-thesaurus--api-key "23ed2cad-ce64-4ab1-abd9-774760e6842d")
  (global-set-key (kbd "<f9> t") 'mw-thesaurus-lookup-dwim)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings for program mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAGS
;; (setq tags-table-list
;;       '("~/code/TAGS/R/"
;;         "~/code/TAGS/PYTHON/"
;;         "~/code/TAGS/C/"
;;         "~/code/TAGS/FORTRAN/"))

(dolist (hook '(after-text-mode-hook ess-mode-hook python-mode-hook c-mode-hook c++-mode-hook inferior-ess-mode-hook))
  (add-hook hook #'(lambda () (xref-etags-mode))))

;; Highlight doxygen mode
(highlight-doxygen-global-mode 1)
(dolist (hook '(c-mode-hook c++-mode-hook python-mode-hook ess-r-mode-hook))
  (add-hook hook #'(lambda () (highlight-doxygen-mode))))

;; Add font lock keywords
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
           ("\\<\\(DEPENDS\\):" 1 font-lock-warning-face t)
           ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
           ("\\<\\(DATE\\):" 1 font-lock-warning-face t)
           ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
           ("\\<\\(DEBUG\\):" 1 font-lock-warning-face t)
           ;;output values high light at comments
           ("\\(\\\\item[ \t]+{[\._A-Za-z0-9]+}\\)" 1 font-lock-warning-face t)
           ("\\<\\([\._A-Za-z0-9]+\$[\-\._A-Za-z0-9]+\\):" 1 font-lock-warning-face t))))
      '(text-mode latex-mode html-mode emacs-lisp-mode
                  python-mode c-mode ess-mode julia-mode markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :ensure t
  :config
  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
  (setq imenu-auto-rescan t)

  ;; (setq auto-mode-alist (cons '("\\.Rmd" . markdown-mode) auto-mode-alist))
  ;; (autoload 'gfm-mode "markdown-mode" "Major mode for editing GitHub Flavored Markdown files" t)
  ;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  )


(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil)
  (global-set-key (kbd "C-c t") 'imenu-list-smart-toggle)
  )

(use-package pandoc-mode
  :ensure t
  :config
  (dolist (hook '(text-mode-hook LaTeX-mode-hook org-mode-hook markdown-mode-hook))
    (add-hook hook (lambda () (pandoc-mode))))
  (add-hook 'pandoc-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c p") 'pandoc-main-hydra/body)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'org-mode-hook
          #'(lambda ()
              (setq org-file-apps
                    (quote
                     ((auto-mode . emacs)
                      ("\\.x?html?\\'" . default)
                      ("\\.pdf\\'" . "evince %s"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BibTeX
(setq bibtex-align-at-equal-sign t)
(setq bibtex-maintain-sorted-entries t)
(setq bibtex-autoadd-commas t)
(defun bibtex-mode-setup ()
  (setq-local fill-prefix ""))
(add-hook 'bibtex-mode-hook #'bibtex-mode-setup)
(add-hook 'bibtex-mode-hook
          #'(lambda ()
              (local-set-key (kbd "C-M-\\") 'bibtex-reformat)))

(defun bibtex-reset-fill-prefix (orig-func &rest args)
  (let ((fill-prefix (make-string (1+ bibtex-text-indentation) ? )))
    (apply orig-func args)))
(advice-add 'bibtex-clean-entry :around #'bibtex-reset-fill-prefix)

;; AUCTEX
(use-package tex
  :defer t
  :ensure auctex
  :config

  ;; Force to enable LaTeX-mode when working with .tex files
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

  (setq TeX-save-query  nil )

  ;; LaTeX AUCTeX features
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (setq LaTeX-math-menu-unicode t)

  ;; Special Environment
  (setq LaTeX-document-regexp "document\\|refsection\\|frontmatter")
  ;; Add listings to verbatim environments
  ;; (add-to-list 'LaTeX-verbatim-environments "lstlisting")

  ;; Set default TeX engine
  (setq TeX-PDF-mode t)
  (setq-default TeX-engine 'xetex) ;this can be set locally

  ;; LaTeX symbols for for TeX mode
  ;; (defun my-latex-mode-setup ()
  ;;   (setq-local company-backends
  ;;               (append '((company-math-symbols-latex company-latex-commands))
  ;;                       company-backends)))
  ;; (add-hook 'TeX-mode-hook 'my-latex-mode-setup)

  ;; LATEXMK integration
  (use-package auctex-latexmk)
  ;; (auctex-latexmk-setup) ; not needed auctex-latexmk-pvc already called.
  (use-package auctex-latexmk-pvc
    :ensure nil
    :config
    (auctex-latexmk-pvc-setup)

    ;; Make sure preview is always viable for PDF file in LatexMkpvc.
    ;; (setq TeX-view-program-selection
    ;;       '((output-pdf "Evince")))
    ;; (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
    ;; (setq TeX-output-extension "pdf")
    ;; (setq TeX-PDF-mode t)

    ;; Replace LaTeX with latexmk -pvc
    (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "LatexMkPvc")))
    )

  ;; Other settings
  (remove-hook 'LaTeX-mode-hook #'auto-fill-mode)

  ;; LaTeX-mode-hook
  (add-hook 'LaTeX-mode-hook
            #'(lambda ()

                (TeX-fold-mode 1)

                ;; Clean all intermediate files, like 'latexmk -c'
                (local-unset-key (kbd "C-c C-k"))
                (local-set-key (kbd "C-c C-k") (lambda () (interactive) (TeX-clean t)))
                (local-set-key (kbd "<f5>") 'TeX-command-run-all)

                (local-set-key (kbd "<f9> (") (lambda () (interactive) (insert "\\left( \\right)")))
                (local-set-key (kbd "<f9> [") (lambda () (interactive) (insert "\\left[ \\right]")))
                (local-set-key (kbd "<f9> {") (lambda () (interactive) (insert "\\left\\{ \\right\\}")))
                (local-set-key (kbd "<f9> |") (lambda () (interactive) (insert "\\left| \\right|")))
                (local-set-key "\$" 'skeleton-pair-insert-maybe)

                ;; This was `c-c c-f c-a`
                (setq LaTeX-font-list
                      '((?a ""              ""  "\\mathcal{"    "}")
                        ;; (?b "\\textbf{"     "}" "\\mathbf{"     "}")
                        (?b "\\textbf{"     "}" "\\bm{"     "}")
                        (?c "\\textsc{"     "}")
                        (?e "\\emph{"       "}")
                        (?f "\\textsf{"     "}" "\\mathsf{"     "}")
                        (?i "\\textit{"     "}" "\\mathit{"     "}")
                        (?l "\\textulc{"    "}")
                        (?m "\\textmd{"     "}")
                        (?n "\\textnormal{" "}" "\\mathnormal{" "}")
                        (?r "\\textrm{"     "}" "\\mathrm{"     "}")
                        (?s "\\textsl{"     "}" "\\mathbb{"     "}")
                        (?t "\\texttt{"     "}" "\\mathtt{"     "}")
                        (?u "\\textup{"     "}")
                        (?w "\\textsw{"     "}")
                        (?d "" "" t)))

                ;; Use \bm{} to repace \mathbf{}
                ;; (add-to-list 'LaTeX-font-list
                ;;              '(m "\\bm{" "}"))
                ))

  ;; Translate key § to ` so both can be used as a math abbreviation
  ;; Drawback, could not type § anymore. Make it locally?
  (keyboard-translate ?§ ?`)
  (setq LaTeX-math-abbrev-prefix "`")

  (setq TeX-source-correlate-mode  t)
  (setq TeX-source-correlate-start-server nil)
  (setq TeX-debug-warnings t)

  ;; Parse on load/save
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)

  (setq TeX-source-correlate-method (quote synctex)) ;only for evince
  (setq TeX-view-evince-keep-focus t)

  ;; RefTeX
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (remove-hook 'reftex-mode #'display-line-numbers-mode)
  (add-hook 'reftex-toc-mode-hook #'visual-line-mode)
  (setq reftex-toc-follow-mode t)
  (setq reftex-revisit-to-follow t)
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.3)

  ;; Enable auto-revert-mode for .bib files
  (add-to-list 'auto-mode-alist
               '("\\.bib\\'" . (lambda ()
                                 (bibtex-mode)
                                 (auto-revert-mode 1))))


;; Extra keybinds for RefTeX
  ;; (setq reftex-extra-bindings t) ;; equavalent as below
  (add-hook 'reftex-load-hook
            #'(lambda ()
                (define-key reftex-mode-map (kbd "C-c l") 'reftex-label)
                (define-key reftex-mode-map (kbd "C-c r") 'reftex-reference)
                (define-key reftex-mode-map (kbd "C-c c") 'reftex-citation)
                (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
                (define-key reftex-mode-map (kbd "C-c s") 'reftex-search-document)
                )
            )

  ;; Add company-reftex backends
  (use-package company-reftex)

  ;; disable reftex-parse-all for labels
  ;; https://github.com/TheBB/company-reftex/pull/13
  (setq company-reftex-labels-parse-all nil) ;; speed up

  (add-hook 'LaTeX-mode-hook
            #'(lambda ()
                (make-local-variable 'company-backends)
                (setq company-backends (copy-tree company-backends))
                (setf (car company-backends)
                      (append '(company-reftex-labels company-reftex-citations) (car company-backends)))

                ))

  (setq reftex-cite-format 'natbib)

  (setq reftex-enable-partial-scans t
        reftex-save-parse-info t
        reftex-use-multiple-selection-buffers t)

  (setq reftex-use-external-file-finders t)
  (setq reftex-external-file-finders
        '(("tex" . "kpsewhich -format=.tex %f")
          ("bib" . "kpsewhich -format=.bib %f")
	  ("bst" . "kpsewhich -format=.bst %f")))


  ;; Perform reftex rescan on save
  (defun my-latex-rescan-on-save ()
    (interactive)
    (reftex-reset-mode)
    (save-buffer))

  (with-eval-after-load 'reftex
    (define-key reftex-mode-map (kbd "C-x C-s") #'my-latex-rescan-on-save))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS (Emacs speaks statistics)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; ESS
(use-package ess
  :ensure t
  :defer t
  :config
  ;; ESS tracebug
  ;; (setq ess-use-tracebug nil)

  ;; Disable flymake, use flycheck instead
  ;; (add-hook 'ess-mode-hook
  ;;           (lambda () (flycheck-mode t)))

  ;; ESS company backends with other backends
  ;; don't auto-insert ess backends
  (setq ess-use-company nil)
  ;; If you want all buffers with the same mode then use company-dabbrev
  (defun my-ess-config ()
    (make-variable-buffer-local 'company-backends)
    (add-to-list 'company-backends
                 '(company-R-library company-R-args company-R-objects company-dabbrev-code :separate)))
  (add-hook 'ess-mode-hook #'my-ess-config)

  ;; disable fancy comments. By default, comments beginning with ‘###’ are aligned to the beginning of the
  ;; line. Comments beginning with ‘##’ are aligned to the current level of indentation for the block containing the
  ;; comment. Finally, comments beginning with ‘#’ are aligned to a column on the right.
  ;; https://github.com/emacs-ess/ESS/issues/1175
  (setf (cdr (assoc 'ess-indent-with-fancy-comments ess-own-style-list)) nil)
  (setq ess-indent-with-fancy-comments nil) ;; take effect only if ess-style is DEFAULT
  (setq ess-style 'OWN)

  ;; R args at start up
  (global-set-key (kbd "<f9> <f6>") 'R) ;; The default R
  (global-set-key (kbd "<f9> r") 'ess-switch-to-end-of-ESS)
  (setq-default inferior-R-args "--no-save --no-restore-data -q")

  ;; Let evaluation not viability to nil, cause emacs hang
  (setq ess-eval-visibly-p nil)

  ;;ESS key binding
  (setq ess-ask-for-ess-directory nil)

  ;; R history files and size
  (setq ess-history-file "~/.Rhistory")

  ;; Add autoloads for R-mode and opening *.R files
  (autoload 'R-mode "ess-site.el" "Major mode for editing R source." t)
  (add-to-list 'auto-mode-alist '("\\.R$" . ess-r-mode))

  ;; Let help on new frame
  ;; (setq ess-help-own-frame t)
  (add-hook 'ess-mode-hook
            #'(lambda ()
                ;; Insert three line comments level-2
                (fset 'my-R-comment-level-2
                      [?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- return
                             ?\C-u ?3 ?# return ?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- up ? ])
                (local-set-key (kbd "<f9> 2") 'my-R-comment-level-2)

                ))


  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:constants . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:%op% . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers)
          (ess-fl-keyword:operators)
          (ess-fl-keyword:delimiters)
          (ess-fl-keyword:=)
          (ess-R-fl-keyword:F&T)))

  (setq ess-eldoc-show-on-symbol t)
  (setq ess-roxy-str "#'")
  (setq ess-use-flymake nil)

  ;; Settings on R shell
  (add-hook 'inferior-ess-mode-hook
            #'(lambda ()
                (define-key inferior-ess-mode-map (kbd "C-c `") 'ess-parse-errors)
                (define-key inferior-ess-mode-map (kbd "C-c d") 'ess-change-directory)
                ;; (define-key inferior-ess-mode-map (kbd "C-c l") 'ess-rutils-load-wkspc))
                ))

  ;; ESS Code styles
  (defun ess-code-style ()
    (local-set-key (kbd "<f9> *") (lambda () (interactive) (insert " %*% ")))
    (local-set-key (kbd "<f9> x") (lambda () (interactive) (insert " %x% ")))
    (local-set-key (kbd "<f9> n") (lambda () (interactive) (insert " %in% "))))
  (add-hook 'ess-mode-hook #'ess-code-style)
  (add-hook 'inferior-ess-mode-hook #'ess-code-style))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Julia mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package julia-mode
  :ensure t
  :config
  (flycheck-julia-setup)
  ;; (add-to-list 'flycheck-global-modes 'julia-mode)
  ;; (add-to-list 'flycheck-global-modes 'ess-julia-mode)
  )


(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "<f5>") 'compile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package python
  :ensure t
  :config
  (setq python-shell-interpreter "python3")
  ;; (setq python-shell-interpreter-args "-i -c 'import sys; print(\"Using \"+sys.executable); print(\"sys.path: \"); [print(p) for p in sys.path]' ")
  (setq python-shell-interpreter-args "-i -c \"import os; print('os.getcwd:', os.getcwd()); import sys; print('sys.executable:' ,sys.executable)\" ")
  (setq python-shell-completion-native-enable nil)

  ;; Enter to indent in python.el
  (define-key python-mode-map (kbd "C-m") 'newline-and-indent)

  ;; Indent/unindent
  (define-key python-mode-map (kbd "C-c >") 'python-indent-shift-right)
  (define-key python-mode-map (kbd "C-c <") 'python-indent-shift-left)

  ;; Disable "python-shell-send-buffer" default binding.
  (define-key python-mode-map (kbd "C-c C-c") nil)
  (define-key python-ts-mode-map (kbd "C-c C-c") nil)

  (define-key python-mode-map (kbd "C-c M-r") 'python-shell-send-region)

  (add-hook 'python-mode-hook
            #'(lambda ()
                ;; Enable flycheck mode
                (flycheck-mode t)
                (defun my-python-send-line-and-step (beg end)
                  (interactive "r")
                  (if (eq beg end)
                      (python-shell-send-region (line-beginning-position) (line-end-position))
                    (python-shell-send-region beg end))
                  (next-line))
                (local-set-key (kbd "C-c C-n") 'my-python-send-line-and-step)


                ;; ElDoc for Python in the minor buffer
                (add-hook 'python-mode-hook #'turn-on-eldoc-mode)

                (defun python-add-breakpoint ()
                  (interactive)
                  (newline-and-indent)
                  (insert "import pdb; pdb.set_trace()"))
                (add-hook 'python-mode-hook
                          #'(lambda ()
                              (define-key python-mode-map
                                          (kbd "C-c C-t") 'python-add-breakpoint)))

                ))
  )


(use-package elpy
  :ensure t
  :config
  (elpy-enable)

  ;; Enable elpy for python ts mode.
  (add-hook 'python-ts-mode-hook 'elpy-enable)

  ;; Disable elpy's flymake, use flycheck
  (remove-hook 'elpy-modules #'elpy-module-flymake)
  (define-key elpy-mode-map (kbd "C-c C-p") nil)

  ;; elpy rpc
  (setq elpy-rpc-virtualenv-path (concat (getenv "HOME") "/.virtualenvs/elpy/"))
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (setq elpy-syntax-check-command "flake8")

  ;; (remove-hook 'elpy-modules 'elpy-module-pyvenv)
  (remove-hook 'elpy-modules #'elpy-module-highlight-indentation)
  (define-key elpy-mode-map (kbd "C-c C-n") nil)

  (define-key elpy-mode-map (kbd "C-c C-c") 'elpy-shell-send-group-and-step)
  (define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer-and-step)

  ;; Alternatives to elpy-goto-definition and fallback to rgrep
  (defun elpy-goto-definition-or-rgrep ()
    "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
    (interactive)
    (if (version< emacs-version "25.1")
        (ring-insert find-tag-marker-ring (point-marker))
      (xref-push-marker-stack))
    (condition-case nil (elpy-goto-definition)
      (error (elpy-rgrep-symbol
              (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
  (define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)


  ;; auto-format your code with "C-c C-r f" on save
  ;; (add-hook 'elpy-mode-hook (lambda ()
  ;;                             (add-hook 'before-save-hook
  ;;                                       'elpy-format-code nil t)))

  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language server mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :after company
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-diagnostics-provider :flycheck) ; could be :none
  (lsp-diagnostics-flycheck-default-level 'warning)

  ;;(lsp-clients-pylsp-library-directories "~/.virtualenvs/elpy/")
  (lsp-pylsp-server-command "~/.virtualenvs/elpy/bin/pylsp")
  (lsp-pylsp-plugins-flake8-enabled t)
  (lsp-pylsp-plugins-flake8-config "~/.config/flake8/tox.ini")
  (lsp-pylsp-plugins-black-enabled t)
  (lsp-pylsp-plugins-isort-enabled t) ; auto sort Python imports


  :config
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-server-install-dir (concat (getenv "HOME") "/.config/emacs/lsp-server"))
  (setq lsp-session-file (concat my-auto-save-list "/lsp-session-v1"))
  (setq lsp-restart 'ignore)  ;; How server-exited events must be handled.
  (setq lsp-verify-signature nil) ;; Disable to get metals server (key expired) working
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil) ;; disable cursor hover (keep mouse hover)

  ;; Performance https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 2048 2048)) ;; 2mb
  (setq lsp-use-plists t) ;; export LSP_USE_PLISTS=true
  (setq lsp-idle-delay 0.500)

  ;; (setq lsp-completion-provider :none)

  ;; Only enable certain LSP client and do not ask for server install.
  ;; (setq lsp-enabled-clients '(metals pylsp texlab2 grammarly-ls))
  ;; (setq lsp-enabled-clients '(metals pyls pylsp ruff semgrep-ls grammarly-ls))

  ;;(setq lsp-clients-pylsp-library-directories "~/.virtualenvs/elpy/")
  (setq lsp-pylsp-server-command "~/.virtualenvs/elpy/bin/pylsp")


  (setq lsp-auto-guess-root nil)
  (setq lsp-warn-no-matched-clients nil)

  (define-key lsp-mode-map (kbd "<f4> <f4>") 'lsp-describe-thing-at-point)
  )

;; lsp-treemacs
;; (use-package lsp-treemacs
;;   :ensure t
;;   :defer t
;;   :commands lsp-treemacs-errors-list
;;   :config
;;   (setq lsp-treemacs-sync-mode t)
;;   (setq lsp-treemacs-errors-position-params '((side . right)))
;;   (defvar treemacs-no-load-time-warnings t)
;;   (define-key lsp-mode-map (kbd "<f4> e") 'lsp-treemacs-errors-list)
;;   )

;; Add metals backend for lsp-mode
(use-package lsp-metals
  :ensure t
  :defer t
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))

  :hook (scala-mode . lsp-deferred)

  :config
  (setq lsp-metals-metals-store-path   (concat (getenv "HOME") "/.local/share/coursier/bin/metals"))
  (setq lsp-metals-coursier-store-path (concat (getenv "HOME") "/.local/share/coursier/bin/coursier"))

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SCALA IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
;; (use-package sbt-mode
;;   :commands sbt-start sbt-command
;;   :config
;;   ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
;;   ;; allows using SPACE when in the minibuffer
;;   (substitute-key-definition
;;    'minibuffer-complete-word
;;    'self-insert-command
;;    minibuffer-local-completion-map)
;;    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
;;    (setq sbt:program-options '("-Dsbt.supershell=false"))
;; )

;; lsp with texlab
;;; (use-package lsp-latex
;;;   :after company
;;;   :ensure t
;;;   :defer t
;;;   :hook (latex-mode . (lambda ()
;;;                        (require 'lsp-latex)
;;;                        (lsp-deferred)))  ; or lsp
;;;   )

;; grammerly for lsp
;; (use-package lsp-grammarly
;;   :ensure t
;;   :defer t
;;   ;; Comment out to start manually
;;   :hook ((latex-mode org-mode)
;;          . (lambda ()
;;              (require 'lsp-grammarly)
;;              (lsp-deferred)))  ;; or lsp

;;   :config
;;   (setq lsp-grammarly-active-modes '(latex-mode org-mode))
;;   (setq lsp-grammarly-auto-activate nil)
;;   (setq lsp-grammarly-domain "academic")
;;   (setq lsp-grammarly-user-words (concat (getenv "HOME") "/.hunspell_en_US"))
;;   )

;; Proxy https://repo.or.cz/proxy-mode.git
(use-package proxy-mode
  :ensure t
  :defer t
  :custom ((proxy-mode-emacs-http-proxy
            '(("http"  . "127.0.0.1:7890")
              ("https" . "127.0.0.1:7890")
              ;; NOTE: don't use `localhost', avoid local server like robe no response
              ("no_proxy" . "127.0.0.1")))
           (proxy-mode-emacs-socks-proxy '("Default server" "127.0.0.1" 7890 5)))

  :config
  (setq proxy-mode-proxy-type 'emacs-url-proxy)
  ;; :commands (proxy-mode)
  )

;; GPT Interface
(use-package gptel
  :ensure t
  :defer t

  :custom
  (gptel-rewrite-highlight-face ((t (:extend t :background "brightblack"))))
  (gptel-use-curl nil) ;; Hotfix for a bug
  ;; (gptel-stream nil)

  :config
  (global-set-key (kbd "<f9> c") 'gptel)
  (global-set-key (kbd "<f9> w") 'gptel-rewrite-menu)

  ;; Checks if the opened file has a `GPT.md` extension and enables `my-minor-mode` when it does.
  (defun my-enable-minor-mode ()
  "Enable my-minor-mode for specific files."
  (when (string-equal buffer-file-name "GPT.md")
    (proxy-mode 1)
    (gptel-mode 1)))  ; Enable minor mode for particular file
  (add-hook 'find-file-hook 'my-enable-minor-mode)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configurations that need to run in the end of init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tree-sitter mode
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-extra-load-path (list (concat my-base-save-list "tree-sitter/")))

  :config
  (treesit-auto-add-to-auto-mode-alist 'all)

  (global-treesit-auto-mode)
  )

(use-package envrc
  :hook (after-init . envrc-global-mode)
  ;; Make sure direnv is installed via https://direnv.net/
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-A ((t (:extend t :background "grey20"))))
 '(ediff-even-diff-B ((t (:extend t (:inherit ediff-even-diff-A)))))
 '(ediff-odd-diff-A ((t (:extend t :background "grey40"))))
 '(ediff-odd-diff-B ((t (:extend t (:inherit ediff-odd-diff-A)))))
 '(font-latex-math-face ((t (:foreground "dark orange"))))
 '(font-latex-sectioning-5-face ((t (:foreground "deep sky blue" :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "light slate blue"))))
 '(gptel-rewrite-highlight-face ((t (:extend t :background "brightblack"))))
 '(highlight ((t (:background "cyan" :foreground "black"))))
 '(highlight-doxygen-comment ((t (:inherit highlight))))
 '(line-number-current-line ((t (:background "black" :slant italic))))
 '(region ((t (:extend nil :background "blue"))))
 '(smerge-lower ((t (:extend t :background "green"))))
 '(smerge-upper ((t (:extend t :background "brightred"))))
 '(tty-menu-enabled-face ((t (:background "dimgray" :foreground "white" :weight bold)))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(provide 'init.el)
;;; init.el ends here
