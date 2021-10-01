;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feng Li's .emacs configurations
;;
;; Copyright: Feng Li <http://feng.li/>
;;
;; Download: https://github.com/feng-li/.emacs.d/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Added by Package.el.  This must come before configurations of installed packages.
;; Don't delete this line.  If you don't want it, just comment it out by adding a
;; semicolon to the start of the line.  You may delete these explanatory comments.  Add
;; MELPA repository
;;; Code:
;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("http" . "127.0.0.1:41091")
;;         ("https" . "127.0.0.1:41091")))
(require 'package)
(setq package-archives '
      (
       ;; ("melpa" . "https://melpa.org/packages/")
       ("gnu-elpa-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
       ("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
       ))
(package-initialize)

;; Add personal load path recursively in front of the default load path
(let ((default-directory (concat user-emacs-directory  "site-lisp")))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Add path for auto saved files
(defvar my-auto-save-list (concat (getenv "HOME") "/.config/.emacs.d/auto-save-list"))
(unless (file-directory-p my-auto-save-list) (make-directory my-auto-save-list t))
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
 '(ess-R-font-lock-keywords
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
 '(ess-eldoc-show-on-symbol t)
 '(ess-roxy-str "#'")
 '(ess-use-flymake nil)
 '(global-display-line-numbers-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(highlight-doxygen-commend-start-regexp
   "\\(/\\*\\(!\\|\\*[^*]\\)\\|#\\('\\)\\|##\\('\\)\\|//\\(!\\|'\\|/[^/
]\\)\\)")
 '(hl-paren-background-colors '("light gray" "steel blue" "lime green" "orange1"))
 '(indicate-empty-lines nil)
 '(neo-window-width 40)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(lexic pandoc-mode wordnut synosaurus yaml-mode mw-thesaurus unfill powerthesaurus julia-mode auctex-latexmk neotree flycheck-grammarly format-all adaptive-wrap highlight-doxygen company-reftex electric-operator elpy markdown-mode dracula-theme yasnippet-snippets flycheck-julia math-symbol-lists langtool polymode company-auctex company-math goldendict writegood-mode highlight-symbol color-theme-solarized popup iedit yasnippet magit ess dash auctex with-editor magit-popup))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1)
 '(send-mail-function 'mailclient-send-it)
 '(session-use-package t nil (session))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil)
 '(warning-suppress-types '((undo discard-info))))

(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; Additional library loaded during start up.
;; (setq tramp-ssh-controlmaster-options nil)
(require 'iso-transl) ;; keyboard input definitions for ISO 8859/1
(require 'session)
(require 'dired-x)
(require 'ispell)
(require 'ibuffer)
(require 'ido)
;(require 'comint)
(require 'org)
(require 'markdown-mode)
;(require 'poly-R)
;(require 'poly-markdown)
(require 'flycheck)
(require 'flycheck-grammarly)
(require 'company)
(require 'ess-site)
(require 'julia-mode)
(require 'flycheck-julia)
(require 'python)
;; (require 'flymake-python-pyflakes)

(load "auctex.el" nil t t)
(require 'company-auctex)
;;(load "preview-latex.el" nil t t)
(require 'langtool)
(require 'writegood-mode)
(require 'yasnippet)
(require 'yasnippet-snippets)
(require 'goldendict)
(require 'electric-operator)
(require 'iedit)
(require 'magit)
(require 'yaml-mode)
(require 'synosaurus)
(require 'mw-thesaurus)
(require 'lexic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Personal information
(setq frame-title-format "%b")
(setq user-full-name "Feng Li")
(setq user-mail-address "m@feng.li")

;; Environment variables
(setenv "OMP_NUM_THREADS" "1")
(setenv "PATH" (concat (concat (getenv "HOME") "/.local/bin:") (getenv "PATH")))

;; Term
(add-to-list 'term-file-aliases '("dumb" . "xterm-256color"))
;; Allow shift-arrow keys and control-arrow keys under different tty
;; Set export TERM="xterm" in .bashrc and
;; term "xterm" in .screenrc.
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;; Theme
(setq dracula-use-24-bit-colors-on-256-colors-terms t)
(unless (display-graphic-p)
  (set-face-background 'default "black" nil)
  )
(load-theme 'dracula t)

;; The scratch settings
(setq initial-scratch-message nil) ;; Disable scratch information
(setq inhibit-startup-message t) ;;stop start up message
(setq fundamental-mode 'text-mode)
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

;; Switch to previous buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "ESC <f2>") 'switch-to-previous-buffer)

;;Use y-n for short
(fset 'yes-or-no-p 'y-or-n-p)

;; Set Fonts
(add-to-list 'default-frame-alist '(font . "M+ 1mn-9.5")) ;
;; (when (display-graphic-p)
;;   (if (> (display-pixel-height) 1080) ;; HDPi
;;       (progn
;;         (add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK SC")) ;
;;         ;; (add-to-list 'default-frame-alist '(font . "M+ 1mn-9")) ;
;;         )
;;     (add-to-list 'default-frame-alist '(font . "Noto Sans Mono CJK SC")) ;
;;     ;; (add-to-list 'default-frame-alist '(font . "M+ 1mn-10")) ;
;;     )
;;   ;; (setq face-font-rescale-alist '(("Noto Sans CJK SC". 1.2)))
;;   ;; (set-fontset-font "fontset-default" 'unicode '("Microsoft YaHei" . "unicode-bmp"))
;;   )
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
(if (display-graphic-p)
    (progn
      ;; Do nothing with window system
      )
  (global-unset-key [?\e ?\e ?\e])
  (global-set-key [?\e ?\e escape] 'keyboard-escape-quit)
  )

(global-display-line-numbers-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Let Alt key be the meta key
(setq x-alt-keysym 'meta)

;; Global visual line mode with better indentation
(setq-default adaptive-wrap-extra-indent 0)
(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(global-visual-line-mode t)

;; (add-hook 'prog-mode-hook '(flyspell-prog-mode -t))

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

;; auto-fill mode
(setq-default fill-column 90)
(dolist (hook '(after-text-mode-hook
                LaTeX-mode-hook
                markdown-mode-hook
                message-mode-hook
                org-mode-hook
	        mail-mode-hook
                ess-mode-hook))
  (add-hook hook '(lambda () (auto-fill-mode 1))))

;; Unfilling a region joins all the lines in a paragraph into a single line for each
;; paragraphs in that region. It is the contrary of fill-region.
(eval-after-load "unfill"
  '(progn
     (define-key global-map (kbd "C-M-q") 'unfill-region)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copy with other applications
(setq select-enable-clipboard t)
(global-set-key (kbd "<mouse-2>") 'clipboard-yank)

;; Keybind to browse the kill ring
(global-set-key (kbd "C-c y")
                '(lambda ()
                   (interactive)
                   (popup-menu 'yank-menu)))
(setq ring-bell-function (lambda ()  t))

;; Disable capitalize-key, conflict with tmux prefix ESC.
(global-unset-key (kbd "M-c"))
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
;; Follow mode (dual pages display)
(global-set-key (kbd "C-<f2>")  'follow-delete-other-windows-and-split)
;; Control-tab to switch among buffers
(global-set-key (kbd "C-<tab>") 'next-buffer)

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
(eval-after-load "session"
  '(progn
     (setq session-use-package nil)
     (add-hook 'after-init-hook 'session-initialize)
     ;; Save sessions with customization
     (setq session-save-file (concat my-auto-save-list "/session-save-file.el"))
     ))

;; Eshell
(setq eshell-directory-name (concat my-auto-save-list "/eshell"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hide and Show code blocks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (hook '(after-text-mode-hook
                c-mode-hook
                org-mode-hook
                python-mode-hook
	        mail-mode-hook
                ess-mode-hook))
  (add-hook hook '(lambda () (hs-minor-mode))))
(global-set-key (kbd "M-+") 'hs-toggle-hiding)
(global-set-key (kbd "M-*") 'hs-show-all)


;; Electric operators
(dolist (hook '(python-mode-hook
                c-mode-hook
                c++-mode-hook
                LaTeX-mode-hook
                ess-r-mode-hook))
  (add-hook hook '(lambda () (electric-operator-mode 1))))
(apply #'electric-operator-add-rules-for-mode 'inferior-python-mode
       (electric-operator-get-rules-for-mode 'python-mode))
(setq electric-operator-R-named-argument-style "spaced")
(electric-operator-add-rules-for-mode 'prog-mode (cons "*" nil))
(electric-operator-add-rules-for-mode 'prog-mode (cons "/" nil))
(electric-operator-add-rules-for-mode 'prog-mode (cons "?" nil))


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

(eval-after-load "yaml-mode"
  '(progn
     (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language and writing https://languagetool.org/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; LanguageTool https://languagetool.org/download/
(eval-after-load "langtool"
  '(progn
     (setq langtool-language-tool-jar "~/.APP/LanguageTool/languagetool-commandline.jar")
     (setq langtool-default-language "en-US")

     (global-set-key (kbd "<f9> l") 'langtool-check)
     (global-set-key (kbd "<f9> L") 'langtool-check-done)

     ;; Show LanguageTool report automatically by popup
     (defun langtool-autoshow-detail-popup (overlays)
       (when (require 'popup nil t)
         ;; Do not interrupt current popup
         (unless (or popup-instances
                     ;; suppress popup after type `C-g` .
                     (memq last-command '(keyboard-quit)))
           (let ((msg (langtool-details-error-message overlays)))
             (popup-tip msg)))))
     (setq langtool-autoshow-message-function
           'langtool-autoshow-detail-popup)))

(eval-after-load "writegood-mode"
  '(progn
     (global-set-key (kbd "<f9> w") 'writegood-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General IDE settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; iedit mode
(eval-after-load "iedit"
  '(progn
     (global-set-key (kbd "C-c i") 'iedit-mode)
     ))

;; Ibuffer mode
(eval-after-load "ibuffer"
  '(progn
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
                 (setq directory-free-space-args "-h")
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
                 (ibuffer-switch-to-saved-filter-groups "default")))))

;; Ido mode
(eval-after-load "ido"
  '(progn
     (ido-mode t)
     (setq ido-use-virtual-buffers nil)
     (setq ido-enable-flex-matching nil)
     (setq ido-ignore-extensions t)
     (setq ido-save-directory-list-file (concat my-auto-save-list "/ido-save-directory-list-file.el"))
     (setq ido-ignore-files ; this also works with directories with c-x c-f
           '("\\.Rc$" "\\.dvi$" "\\.pdf$" "\\.ps$" "\\.out$" "\\.fls$" "\\.spl$" "\\.fff$"
             "\\.ttt$" "\\.log$" "\\.ods$" "\\.eps$" "\\#$" "\\.png$" "\\~$" "\\.RData$"
             "\\.nav$" "\\.snm$" "\\`\\.\\./" "\\`\\./" "\\.synctex.gz$" "\\.fdb_latexmk$"
             "\\.tar.gz$" "\\.zip$" "\\.o$" "\\.tar$" "\\.Rproj$" "\\.Rcheck$" "\\.doc$"
             "\\.docx$" "\\.Rhistory$" "auto/" "__pycache__/" "\\.bcf$" "\\.run.xml$" "_region_.tex$"
             "\\.xdv$" "\\.DS_Store$" "\\.cfg$" "\\.bak$" "\\.gitignore"))

     (setq  ido-ignore-directories ; only works with ido-dired
            '("\\`auto/" "\\.prv/" "\\`CVS/" "\\`.git/" "\\`.ropeproject/" "\\`\\.\\./"
              "\\`\\./" "\\`_bookdown_files/" "__pycache__/"))

     (setq ido-ignore-buffers
           '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer" "*scratch*"
             "^\\*Ibuffer*" "^\\*ESS-errors*" "^\\*Warnings*" "*TeX Help*"
             "*Pymacs*" "*Flymake log*" "\\.log$" "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp"
             "^\\*cvs-" "_region_" "^TAGS$" "^\\*Ido" "^\\*.*dictem buffer\\*$"
             "^\\*inferior-lisp*" "^\\*Compile-Log\\*" "*output*" "^.*output\\*$"))

     (defun ido-kill-emacs-hook () (ignore-errors (ido-save-history)))
     ))

;; ElDoc mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Comint for input history and  scrolling
(eval-after-load "comint"
  '(progn
     (define-key
       comint-mode-map (kbd "C-<up>")'comint-previous-matching-input-from-input)
     (define-key
       comint-mode-map (kbd "C-<down>") 'comint-next-matching-input-from-input)
     (define-key
       comint-mode-map (kbd "C-k") 'comint-kill-input)

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
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto completion settings (company mode, yasnippet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "yasnippet"
  '(progn
     (yas-global-mode 1)
     ;; (setq yas-snippet-dirs
     ;;       '(;; personal snippets
     ;;         "~/.emacs.d/snippets"
     ;;         ;; snippet collection
     ;;         ;; "~/.emacs.d/site-lisp/yasnippet-snippets/snippets"
     ;;         ))
     )
  )

(eval-after-load "company"
  '(progn
     (add-hook 'after-init-hook 'global-company-mode)
     (setq company-minimum-prefix-length 1)

     ;; Add yasnippet support for all company backends
     ;; https://github.com/syl20bnr/spacemacs/pull/179
     (defvar company-mode/enable-yas t
       "Enable yasnippet for all backends.")
     (defun company-mode/backend-with-yas (backend)
       (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
           backend
         (append (if (consp backend) backend (list backend))
                 '(:with company-yasnippet))))
     (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

     ;; Use company quick access number to select candidates. The key want binded to M-numbers.
     ;;https://github.com/abo-abo/oremacs/blob/d2b2cd8371b94f35a42000debef1c2b644cb9472/modes/ora-company.el#L22
     (setq company-show-quick-access t)
     (defun ora-company-number ()
       "Forward to `company-complete-number'.
     Unless the number is potentially part of the candidate.  In
     that case, insert the number."
       (interactive)
       (let* ((k (this-command-keys))
              (re (concat "^" company-prefix k)))
         (if (or (cl-find-if (lambda (s) (string-match re s))
                             company-candidates)
                 (> (string-to-number k)
                    (length company-candidates))
                 (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
             (self-insert-command 1)
           (company-complete-number
            (if (equal k "0")
                10
              (string-to-number k))))))

     ;;https://github.com/abo-abo/oremacs/blob/d2b2cd8371b94f35a42000debef1c2b644cb9472/init.el#L28
     (defun ora-advice-add (&rest args)
       (when (fboundp 'advice-add)
         (apply #'advice-add args)))
     (defun ora--company-good-prefix-p (orig-fn prefix)
       (unless (and (stringp prefix) (string-match-p "\\`[0-9]+\\'" prefix))
         (funcall orig-fn prefix)))
     (ora-advice-add 'company--good-prefix-p :around #'ora--company-good-prefix-p)

     (let ((map company-active-map))
       (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
             (number-sequence 0 9)))
     ))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t) ;; Highlight parentheses

(eval-after-load "highlight-parentheses"
  '(progn
     (define-globalized-minor-mode global-highlight-parentheses-mode
       highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
     (global-highlight-parentheses-mode t))) ;; Highlight symbols

;; parentheses mode
(show-paren-mode t)
;; (setq show-paren-style 'expression) ;; highlight whole block

(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "\'" 'skeleton-pair-insert-maybe)

;; Commenting
(global-set-key (kbd "M-3") 'comment-or-uncomment-region)

;; Add extra info path
(eval-after-load "info-look"
  '(progn
     (add-to-list
      'Info-default-directory-list (concat user-emacs-directory "info"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq-default ediff-ignore-similar-regions t)


(setq vc-handled-backends ()) ;; Disable vc-git and use magit
(vc-mode -1)
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               (concat user-emacs-directory "/magit/Documentation/"))
  )
(setq vc-follow-symlinks nil)

(eval-after-load "magit"
  '(progn
     ;; Save transient file with customization
     (setq transient-history-file (concat my-auto-save-list "/transient-history-file.el"))

     ;; magit with-editor support
     (define-key (current-global-map)
       [remap async-shell-command] 'with-editor-async-shell-command)
     (define-key (current-global-map)
       [remap shell-command] 'with-editor-shell-command)

     (define-key with-editor-mode-map (kbd "C-c C-c") nil)

     )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling checking & dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spelling Check
(eval-after-load "ispell"
  '(progn
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

     (global-set-key (kbd "<f9> 4") 'ispell-word)))

;; Auto correct spelling mistakes
(global-set-key (kbd "<f9> c") 'flyspell-auto-correct-word)

(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "C-d") nil)
  )

;; FlyCheck
;; (add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load "synosaurus"
  '(progn
     (dolist (hook '(text-mode-hook
                     latex-mode-hook
                     LaTeX-mode-hook
                     prog-mode-hook
                     org-mode-hook
                     markdown-mode-hook))
       (add-hook hook (lambda () (synosaurus-mode))))

     (setq synosaurus-choose-method 'popup) ; popup, ido
     ;; (setq synosaurus-backend  'Wordnet) ; apt install wordnet
     (define-key synosaurus-mode-map (kbd "<f4>") 'synosaurus-choose-and-replace)
     )
  )


;; Fly spell performance
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

;; Fly spell mode for major mode
(dolist (hook '(text-mode-hook
                latex-mode-hook
                markdown-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(add-hook 'tex-mode-hook (function (lambda () (setq ispell-parser 'tex))))

;; Disable flyspell for special modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Fly spell check comments for a programmer
;; (dolist (hook '(emacs-lisp-mode-hook
;;                 c-mode-hook
;;                 c++-mode-hook
;;                 ess-mode-hook
;;                 python-mode-hook))
;;   (add-hook hook (lambda () (flyspell-prog-mode))))

;; Moby Thesaurus II
;; (setq synonyms-file        "~/.emacs.d/hunspell/mobythesaurus/mthesaur.txt")
;; (setq synonyms-cache-file  "~/.emacs.d/hunspell/mobythesaurus/mthesaur.txt.cache")
;; (require 'synonyms)

;; Golden Dictionary
;; (eval-after-load "goldendict"
;;   '(progn
;;      (global-set-key (kbd "C-c d") 'goldendict-dwim)))

;; StarDict
;; apt install sdcv
;; https://github.com/Dushistov/sdcv
(eval-after-load "lexic"
  '(progn
     (setq lexic-dictionary-path (concat user-emacs-directory "dict/sdcv/"))
     (setq lexic-dictionary-list
           '(;; "Soule's Dictionary of English Synonyms (En-En)"
             "Merriam-Webster's Collegiate Thesaurus (En-En)"
             ;; "Merriam-Webster's Advanced Learner's Dictionary (En-En)"
             "Longman Dictionary of Common Errors (En-En)"))
     (global-set-key (kbd "<f9> d") 'lexic-search)
     ))

(eval-after-load "mw-thesaurus"
  '(progn
     (setq mw-thesaurus--api-key "23ed2cad-ce64-4ab1-abd9-774760e6842d")
     (global-set-key (kbd "<f9> t") 'mw-thesaurus-lookup-dwim)
     )
  )


(add-hook 'c-mode-common-hook
          (lambda () (define-key c-mode-base-map (kbd "<f5>") 'compile)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General settings for program mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAGS
(setq tags-table-list
      '("~/code/TAGS/R/"
        "~/code/TAGS/PYTHON/"
        "~/code/TAGS/C/"
        "~/code/TAGS/FORTRAN/"))
(dolist (hook '(after-text-mode-hook
                ess-mode-hook
	        python-mode-hook
	        c-mode-hook
	        c++-mode-hook
                inferior-ess-mode-hook))
  (add-hook hook '(lambda () (xref-etags-mode))))

;; highlight-indent-guides-mode, can make emacs slow with large files
;; (eval-after-load "highlight-indent-guides"
;;   '(progn
;;      (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
;;      (setq highlight-indent-guides-method 'character)
;;      (setq highlight-indent-guides-character ?\│)
;;      (setq highlight-indent-guides-auto-odd-face-perc 30)
;;      (setq highlight-indent-guides-auto-even-face-perc 30)
;;      (setq highlight-indent-guides-auto-character-face-perc 40)
;;      )
;;   )

;; Highlight doxygen mode
(highlight-doxygen-global-mode 1)
(dolist (hook '(c-mode-hook
                c++-mode-hook
                python-mode-hook
                ess-r-mode-hook))
  (add-hook hook '(lambda () (highlight-doxygen-mode))))

;; (add-hook 'prog-mode-hook 'highlight-doxygen-mode)

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
(eval-after-load "markdown-mode"
  '(progn
     (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
     ;; (setq auto-mode-alist (cons '("\\.Rmd" . markdown-mode) auto-mode-alist))

     (autoload 'gfm-mode "markdown-mode"
       "Major mode for editing GitHub Flavored Markdown files" t)
     (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

     ))

     (dolist (hook '(text-mode-hook
                     latex-mode-hook
                     LaTeX-mode-hook
                     prog-mode-hook
                     org-mode-hook
                     markdown-mode-hook))
       (add-hook hook (lambda () (synosaurus-mode))))


(eval-after-load "pandoc"
  '(progn
     (dolist (hook '(text-mode-hook
                     latex-mode-hook
                     LaTeX-mode-hook
                     org-mode-hook
                     markdown-mode-hook))
       (add-hook hook (lambda () (pandoc-mode))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'org-mode-hook
          '(lambda ()
             (setq org-file-apps
                   (quote
                    ((auto-mode . emacs)
                     ("\\.x?html?\\'" . default)
                     ("\\.pdf\\'" . "evince %s"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "auctex.el"
  '(progn
     ;; LaTeX AUCTex features
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (setq LaTeX-math-menu-unicode t)

     (setq LaTeX-document-regexp "document\\|refsection\\|frontmatter")

     ;; Set default TeX engine
     (setq TeX-PDF-mode t)
     (setq-default TeX-engine 'xetex) ;this can be set locally

     (require 'auctex-latexmk)
     ;; (auctex-latexmk-setup) ; not needed auctex-latexmk-pvc already called.
     (require 'auctex-latexmk-pvc)
     (auctex-latexmk-pvc-setup)

     ;; Use latexmkpvc as the main command
     (defun TeX-command-run-latexmkpvc ()
       (interactive)
       (TeX-save-document (TeX-master-file))
       (TeX-command "LatexMkPvc" 'TeX-master-file -1))
     (add-hook 'LaTeX-mode-hook
               '(lambda ()
                  (local-set-key (kbd "<f5>") 'TeX-command-run-all)
                  ))

     ;; Replace LaTeX with latexmk -pvc
     (setcdr (assoc "LaTeX" TeX-command-list)
             '("latexmk -pvc -pv- %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk-pvc nil
               :help "Run LaTeX with LatexMKPvc"))

     ;; Translate key § to ` so both can be used as a math abbreviation
     ;; Drawback, could not type § anymore. Make it locally?
     (keyboard-translate ?§ ?`)
     (setq LaTeX-math-abbrev-prefix "`")

     ;; Allow company-auctex backends
     (company-auctex-init)

     (setq TeX-source-correlate-mode  t)
     (setq TeX-source-correlate-start-server nil)

     ;; Add listings to verbatim environments
     (eval-after-load "latex"
       '(add-to-list 'LaTeX-verbatim-environments "lstlisting"))


     ;; Parse on load/save
     (setq TeX-parse-self t)
     (setq TeX-auto-save t)

     (setq TeX-source-correlate-method (quote source-specials)) ; only for dvi
     (setq TeX-source-correlate-method (quote synctex)) ;only for evince
     (setq bibtex-maintain-sorted-entries t)

     ;; Add short cuts, hold Windows key
     (defun auctex-insert-special ()
       (local-set-key (kbd "<f9> (") (lambda () (interactive) (insert "\\left( ")))
       (local-set-key (kbd "<f9> )") (lambda () (interactive) (insert "\\right)")))

       (local-set-key (kbd "<f9> [") (lambda () (interactive) (insert "\\left[ ")))
       (local-set-key (kbd "<f9> ]") (lambda () (interactive) (insert "\\right]")))

       (local-set-key (kbd "<f9> {") (lambda () (interactive) (insert "\\left\\{ ")))
       (local-set-key (kbd "<f9> }") (lambda () (interactive) (insert "\\right\\}")))

       (local-set-key (kbd "<f9> |") (lambda () (interactive) (insert "\\left| \\right|")))
       (local-set-key (kbd "C-\\") (lambda () (interactive) (insert "\\")))

       ;; Use \bm{} to repace \mathbf{}
       (fset 'my-insert-bold-math
             [?\C-w ?\\ ?b ?m ?\{ ?\C-y ?\} right])
       (local-set-key (kbd "C-c C-x C-b") 'my-insert-bold-math))

     (add-hook 'LaTeX-mode-hook 'auctex-insert-special)

     ;; Enable file-line-error to avoid error message "Error occured after last TeX file closed" ; now is default for 11.89
     ;; (setq LaTeX-command-style (quote (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))

     (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
     (setq TeX-save-query  nil )

     ;; RefTeX
     (setq reftex-plug-into-AUCTeX t)
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (setq reftex-toc-follow-mode t)
     (setq reftex-revisit-to-follow t)
     (setq reftex-toc-split-windows-horizontally t)
     (setq reftex-toc-split-windows-fraction 0.3)

     ;; Extra keybinds
     ;; (setq reftex-extra-bindings t) ;; equavalent as below
     (add-hook 'reftex-load-hook
               '(lambda ()
                  (define-key reftex-mode-map (kbd "C-c t") 'reftex-toc)
                  (define-key reftex-mode-map (kbd "C-c l") 'reftex-label)
                  (define-key reftex-mode-map (kbd "C-c r") 'reftex-reference)
                  (define-key reftex-mode-map (kbd "C-c c") 'reftex-citation)
                  (define-key reftex-mode-map (kbd "C-c v") 'reftex-view-crossref)
                  (define-key reftex-mode-map (kbd "C-c s") 'reftex-search-document)
                  (define-key reftex-mode-map (kbd "C-c g") 'reftex-grep-document)
                  )
               )

     ;; Allow company-reftex backends
     (add-hook 'LaTeX-mode-hook
               '(lambda ()
                  (make-local-variable 'company-backends)
                  (setq company-backends (copy-tree company-backends))
                  (setf (car company-backends)
                        (append '(company-reftex-labels company-reftex-citations) (car company-backends)))
                  ))

     (setq reftex-cite-format 'natbib)
     (setq reftex-use-external-file-finders t)
     (setq reftex-external-file-finders
           '(("tex" . "kpsewhich -format=.tex %f")
             ("bib" . "kpsewhich -format=.bib %f")
	     ("bst" . "kpsewhich -format=.bst %f")))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS (Emacs speaks statistics)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; ESS
(eval-after-load "ess-site"
  '(progn
     ;; ESS tracebug
     ;; (setq ess-use-tracebug nil)
     ;; (require 'ess-rutils)
     ;; (require 'ess-tracebug) ;; ESS tracebug
     ;; (require 'ess-R-object-tooltip)

     ;; Disable flymake, use flycheck instead
     ;; (add-hook 'ess-mode-hook
     ;;           (lambda () (flycheck-mode t)))

     ;; R args at start up
     (global-set-key (kbd "<f9> <f6>") 'R) ;; The default R
     (global-set-key (kbd "<f9> r") 'ess-switch-to-end-of-ESS)
     (setq-default inferior-R-args "--no-save --no-restore-data -q")

     ;; Let ESS Sweave work with AUCTEX
     (setq ess-swv-plug-into-AUCTeX-p t)

     ;; Let evaluation not viability to nil, cause emacs hang
     (setq ess-eval-visibly-p nil)

     ;;ESS key binding
     (setq ess-ask-for-ess-directory nil)

     ;; R history files and size
     (setq ess-history-file "~/.Rhistory")

     ;; Let help on new frame
     ;; (setq ess-help-own-frame t)
     (add-hook 'ess-mode-hook
               '(lambda ()
                  (fset 'my-R-comment-level-1
                        (lambda (&optional arg) "Insert level-1 R comment block"
                          (interactive "p")
                          (kmacro-exec-ring-item
                           (quote ([21 55 57 35 return 21 51 35
                                       return 21 55 57 35 up 32] 0 "%d")) arg)))
                  (local-set-key (kbd "<f9> 1") 'my-R-comment-level-1)

                  ;; Insert three line comments level-2
                  (fset 'my-R-comment-level-2
                        [?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- return
                               ?\C-u ?3 ?# return ?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- up ? ])
                  (local-set-key (kbd "<f9> 2") 'my-R-comment-level-2)

                  ))

     ;; Settings on R shell
     (add-hook 'inferior-ess-mode-hook
               '(lambda ()
                  (define-key inferior-ess-mode-map (kbd "C-c `") 'ess-parse-errors)
                  (define-key inferior-ess-mode-map (kbd "C-c d") 'ess-change-directory)
                  ;; (define-key inferior-ess-mode-map (kbd "C-c l") 'ess-rutils-load-wkspc))
                  ))

     ;; ESS Code styles
     (defun ess-code-style ()
       (local-set-key (kbd "<f9> *") (lambda () (interactive) (insert " %*% ")))
       (local-set-key (kbd "<f9> x") (lambda () (interactive) (insert " %x% ")))
       (local-set-key (kbd "<f9> n") (lambda () (interactive) (insert " %in% "))))
     (add-hook 'ess-mode-hook 'ess-code-style)
     (add-hook 'inferior-ess-mode-hook 'ess-code-style))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Julia mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "julia-mode"
  '(progn
     (flycheck-julia-setup)
     ;; (add-to-list 'flycheck-global-modes 'julia-mode)
     ;; (add-to-list 'flycheck-global-modes 'ess-julia-mode)
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "python"
  '(progn

     (elpy-enable)
     ;; (setq elpy-rpc-virtualenv-path (concat (getenv "HOME") "/.emacs.d/elpy/" system-name "/rpc-venv"))
     ;; (setq elpy-rpc-virtualenv-path (concat (getenv "HOME") "/.cache/elpy/rpc-venv"))
     (setq elpy-rpc-virtualenv-path (concat (getenv "HOME") "/.local"))
     (setq elpy-rpc-python-command "python3")
     (setq elpy-syntax-check-command (concat elpy-rpc-virtualenv-path  "/bin/flake8"))

     ;; Disable elpy's flymake, use flycheck
     (remove-hook 'elpy-modules 'elpy-module-flymake)
     (define-key elpy-mode-map (kbd "C-c C-n") nil)
     (setq flycheck-python-flake8-executable (concat elpy-rpc-virtualenv-path  "bin/python3"))
     (setq flycheck-python-pylint-executable (concat elpy-rpc-virtualenv-path  "bin/python3"))
     (setq pylint-command (concat elpy-rpc-virtualenv-path  "bin/pylint3"))

     ;; (remove-hook 'elpy-modules 'elpy-module-pyvenv)
     (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)

     (define-key elpy-mode-map (kbd "C-c C-f") 'elpy-shell-send-defun-and-step-and-go)
     (define-key elpy-mode-map (kbd "C-c C-r") 'elpy-shell-send-region-or-buffer-and-step-and-go)


     (require 'pydoc)
     (define-key elpy-mode-map (kbd "C-c C-v") 'pydoc)
     ;; (local-set-key (kbd "C-c C-v") 'pydoc) ; C-c C-v was bounded to elpy-check

     (setq python-shell-interpreter "python3")
     (setq python-shell-completion-native-enable nil)

     (add-hook 'python-mode-hook
               '(lambda ()
                  ;; (setq python-python-command "python3")

                  ;; Enable flycheck mode
                  (flycheck-mode t)

                  ;; Enter to indent in python.el
                  (define-key python-mode-map (kbd "C-m") 'newline-and-indent)

                  (define-key python-mode-map (kbd "C-c M-r") 'python-shell-send-region)

                  (defun my-python-send-line-and-step (beg end)
                    (interactive "r")
                    (if (eq beg end)
                        (python-shell-send-region (point-at-bol) (point-at-eol))
                      (python-shell-send-region beg end))
                    (next-line))
                  (local-set-key (kbd "C-c C-n") 'my-python-send-line-and-step)


                  ;; ElDoc for Python in the minor buffer
                  (add-hook 'python-mode-hook 'turn-on-eldoc-mode)

                  (defun python-add-breakpoint ()
                    (interactive)
                    (newline-and-indent)
                    (insert "import pdb; pdb.set_trace()"))
                  (add-hook
                   'python-mode-hook
                   '(lambda ()
                      (define-key python-mode-map
                        (kbd "C-c C-t") 'python-add-breakpoint)))

                  ;; Font-Lock
                  (make-face 'font-lock-special-macro-face)
                  (set-face-background 'font-lock-special-macro-face "magenta")
                  (set-face-foreground 'font-lock-special-macro-face "white")

                  ))
     )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip ((t (:background "dim gray" :foreground "light gray"))))
 '(font-latex-math-face ((t (:foreground "dark orange"))))
 '(font-latex-sectioning-5-face ((t (:foreground "deep sky blue" :weight bold))))
 '(font-latex-sedate-face ((t (:foreground "dark magenta"))))
 '(font-lock-function-name-face ((t (:foreground "deep sky blue" :weight normal))))
 '(highlight-doxygen-comment ((t (:inherit highlight))))
 '(line-number ((t (:inherit t :background nil))))
 '(line-number-current-line ((t (:inherit secondary-selection :slant italic))))
 '(neo-dir-link-face ((t (:inherit font-lock-function-name-face))))
 '(region ((t (:background "dim gray" :foreground "light gray")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(provide '.emacs)
;;; .emacs ends here
