(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(display-time-mode nil)
 '(doc-view-continuous t)
 '(frame-background-mode nil)
 '(global-font-lock-mode t nil (font-lock))
 '(hl-paren-background-colors (quote ("light gray" "steel blue" "lime green" "orange1")))
 ;;'(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(org-support-shift-select t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (flyspell-mode)) (lambda nil (turn-on-auto-fill)) text-mode-hook-identify)))
 '(tool-bar-mode nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all required packages 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add personal load path recursively in front of the default load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append 
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Additional library loaded during start up.
(require 'iso-transl) ;; A bug, could not recognize the tilde key in Swedish keyboard.
(require 'htmlize-view)
(require 'session)
;;(require 'scim-bridge)
;;(require 'ibus)
(require 'ibuffer)
(require 'ido)
(require 'comint)
(require 'org-install)
;; (require 'flymake)
(require 'dictem)
(require 'auto-complete-config)
;;(require 'highlight-parentheses) 
(require 'auto-highlight-symbol) 
;;(require 'yasnippet) 
(require 'info-look) 
(require 'ess-site)
;;(require 'matlab-load)
(require 'egg)
(require 'git-emacs)
(require 'git-blame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set home directory
(setq default-directory "~/workspace/")

(when window-system
  (desktop-save-mode))

;; Default frame height and width
(setq default-frame-alist (append (list
                                   '(width  . 85)  ; Width set to 81 characters
                                   ;;'(height . 55)) ; Height set to 60 lines
                                   default-frame-alist)))

;; Personal info
(setq frame-title-format "%b")
(setq user-full-name "Feng Li")
(setq user-mail-address "feng.li@stat.su.se")

;; Environment variables 
(setenv "PATH" (concat (getenv "PATH") ":~/.bin"))
(setq exec-path (append exec-path '("~/.bin")))
(setenv "OMP_NUM_THREADS" "1")
(setq explicit-bash-args '("--init-file" "~/.bashrc")) 

;; Suspend the compile warnings
(setq byte-compile-warnings nil)

;; Disable scroll-bar
(scroll-bar-mode -1)

;; Enable line number mode when opening new files
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; Let Alt key be the meta key
(setq x-alt-keysym 'meta)

(eval-after-load "ibus"
  '(progn
     (add-hook 'after-init-hook 'ibus-mode-on)
     (ibus-define-common-key ?\M-\s t)
     ;; Change cursor color depending on SCIM status
     (setq ibus-cursor-color '("red" "#00BBBB" "limegreen"))
     ))


;; Suspend and resume hook
(add-hook 'suspend-hook
          (function (lambda ()
                      (or (y-or-n-p
                           "Really suspend emacs?")
                          (error "Suspend canceled.")))))
(add-hook 'suspend-resume-hook
          (function (lambda () (message "Emacs resumed!"))))


;; HTML Print
(eval-after-load "htmlize-view"
  '(progn
     (htmlize-view-add-to-files-menu)))

;; SCIM bridge for Chinese input method
(eval-after-load "scim-bridge"
  '(progn
     ;; Turn on scim-mode automatically after loading .emacs
     ;; (add-hook 'after-init-hook 'scim-mode-on)
     ;; Use M-SPC for activating SCIM
     (scim-define-common-key ?\C-\s t)
     ;; Change cursor color depending on SCIM status
     (setq scim-cursor-color '("red" "#00BBBB" "limegreen"))))

;; Global visual line mode 
(global-visual-line-mode -1)

;; Dired mode
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq dired-omit-files-p t)
(add-hook 'dired-mode-hook
          (lambda ()
            (setq dired-omit-files "^\\.[a-z|A-Z]+\\|^\\.?#\\|^\\.$")
            (setq dired-omit-extensions '(".pyc" "CVS/" "~" ".o" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".dvi" ".fmt" ".tfm" ".class" ".fas" ".lib" ".x86f" ".sparcf" ".lo" ".la" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".idx" ".lof" ".lot" ".glo" ".blg" ".cp" ".cps" ".fn" ".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs" ".Rc"))
            (setq dired-listing-switches "-hla")
            (setq directory-free-space-args "-h")
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "<backspace>")
              (lambda () (interactive) (find-alternate-file "..")))
            (setq cursor-type 'box)
            (dired-omit-mode 1)
            (local-set-key (kbd "C-h") 'dired-omit-mode)))
(put 'dired-find-alternate-file 'disabled nil)


;; Default Height and width
;; (setq default-frame-alist (append (list
;;                                    '(width  . 81)  ; Width set to 81 characters
;;                                    '(height . 55)) ; Height set to 60 lines
;;                                   default-frame-alist))


;; disable tooltips
(tooltip-mode nil)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "<f9> n") 'new-frame)
(global-set-key (kbd "<f9> s") 'rgrep)
(global-set-key (kbd "<f9> l") 'find-name-dired)
(global-set-key (kbd "<f9> q") 'fill-region-as-paragraph)
(global-set-key (kbd "<f9> TAB") 'indent-relative)
(global-set-key (kbd "ESC <f2>") 'next-multiframe-window) ;; Circulate among windows ESC-F2
;; (global-set-key (kbd "M-SPC") 'set-mark-command) ;It was C-SPC 

;; The scratch settings
;; Disable scratch information
(setq initial-scratch-message nil)

(setq fundamental-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode) ;; text mode in scratch
(add-hook 'text-mode-hook 
          (function (lambda () (turn-on-auto-fill)))) ;; Auto fill mode

;; Kill the current buffer, without confirmation.
(fset 'my-kill-current-buffer
      [?\C-x ?k return])
(global-set-key [pause] 'my-kill-current-buffer)

;; Bind undo with the use
(global-set-key (kbd "C-z") 'undo)

;; F2 switch to previous buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "<f2>") 'switch-to-previous-buffer)

;;;set server-start
;;(server-start)

;; Default English fonts
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10"))

;; Chinese fonts
(set-fontset-font "fontset-default"
                  'han '("Adobe Heiti Std" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'cjk-misc '("Adobe Heiti Std" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'bopomofo '("Adobe Heiti Std" . "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'gb18030 '("Adobe Heiti Std". "unicode-bmp"))
(set-fontset-font "fontset-default"
                  'symbol '("Adobe Heiti Std". "unicode-bmp"))


;; Disable menu bar
(menu-bar-mode t)

;;stop start up message
(setq inhibit-startup-message t)

;;Use y-n short
(fset 'yes-or-no-p 'y-or-n-p)

;; Allow shift-arrow keys and control-arrow keys under different tty
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])
  
  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5C" [C-right]))

(if (equal "screen" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])
  
  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5C" [C-right]))


;;shift selection
(setq shift-select-mode t)

;; make typing override text selection
(delete-selection-mode 1) ; 

;; TAB settings
(setq-default indent-tabs-mode nil)

;; Control-tab to switch among buffers TODO: not working within the console   
;; (global-set-key (kbd "C-<tab>") 'next-buffer)

;; Keep buffer order during switch 
;; (require 'flobl)

;;Session(Keep section each time)
(eval-after-load "session"
  '(progn
     (add-hook 'after-init-hook 'session-initialize)))

;; Cursor is bar: Not clear under console
(setq-default cursor-type 'bar)

;; Color-Theme
;; (defvar after-make-console-frame-hooks '()
;;   "Hooks to run after creating a new TTY frame")
;; (defvar after-make-window-system-frame-hooks '()
;;   "Hooks to run after creating a new window-system frame")
;; (defun run-after-make-frame-hooks (frame)
;;   "Selectively run either `after-make-console-frame-hooks' or
;;  `after-make-window-system-frame-hooks'"
;;   (select-frame frame)
;;   (run-hooks (if window-system
;;                  'after-make-window-system-frame-hooks
;;                'after-make-console-frame-hooks)))
;; (add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)
;; (add-hook 'after-init-hook
;;           (lambda ()
;;             (run-after-make-frame-hooks (selected-frame))))
;; (require 'color-theme)
;; (color-theme-initialize)
;; (add-hook 'after-make-window-system-frame-hooks 'color-theme-jonadabian-slate)
;; (add-hook 'after-make-console-frame-hooks 'color-theme-tty-dark)

;;set visible-bell
(setq visible-bell t)

;;set big kill ring
(setq kill-ring-max 150)

;;column and line number
;; (setq column-number-mode t)
;; (global-linum-mode 1)
;; (require 'linum-off)
;; (setq linum-disabled-modes-list 
;;       '(eshell-mode compilation-mode ess-mode))

;;auto fill mode
;;set length of character
(setq default-fill-column 79)
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               'org-mode-hook
               'ess-mode-hook))
  (add-hook hook '(lambda () (auto-fill-mode 1))))


;;copy with other applications 
(setq x-select-enable-clipboard t)

(setq ring-bell-function (lambda ()  t))

;; Matlab support
(eval-after-load "matlab-load"
  '(progn
     (matlab-cedet-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General IDE settings (ElDoc, ECB, Comint...) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speed bar
;; (when window-system
;;   (speedbar t)
;;   (speedbar-add-supported-extension (quote(".R" ".r" ".bib" ".org"))))

;; Ibuffer mode
(eval-after-load "ibuffer"
  '(progn
     (global-set-key (kbd "C-x C-b") 'ibuffer)
     (global-set-key (kbd "<f9> i") 'ibuffer)

     (setq ibuffer-saved-filter-groups
           (quote (("default"
                    ("Scripts" (or
                                (mode . ess-mode)
                                (mode . emacs-lisp-mode)))
                    ("Files" (or
                              (mode . LaTeX-mode)
                              (mode . latex-mode)
                              (mode . org-mode)))
                    ("Proc" (or
                             (mode . inferior-ess-mode)))
                    ("Help" (or
                             (mode . help-mode)
                             (mode . dictem-mode)
                             (mode . ess-help-mode)
                             (mode . Info-mode)))              
                    ("Messages" (or
                                 (name . "^\\*scratch\\*$")
                                 (name . "^\\*Messages\\*$")
                                 (name . "^\\*Completions\\*$")
                                 (mode . fundamental-mode)))
                    ))))
     (add-hook 'ibuffer-mode-hook
               (lambda ()
                 (ibuffer-switch-to-saved-filter-groups "default")))))

;; ido mode
(eval-after-load "ido"
  '(progn
     (ido-mode t)
     (setq ido-use-virtual-buffers nil)
     (setq ido-ignore-files '("\\.Rc$"))     
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
     ))

;; Clear buffer output
(defun comint-clear-buffer ()
  (interactive)
  (goto-char (point-max))
  (move-beginning-of-line nil)
  (backward-char)
  (move-beginning-of-line nil)
  (delete-region (point-min) (point))
  (goto-char (point-max)))
(define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)

;; Replace ^M 
(fset 'my-replace-m
      [escape ?< escape ?% ?\C-q ?\C-m return ?  return ?! escape ?<])

;; Insert Current time, linux only?
(global-set-key "\C-ct" 'my-insert-time)
(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y")))

;; CEDET & ECB
;; (global-ede-mode 1)
;; ;;(semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers)
;; (global-srecode-minor-mode 1)

;; (require 'ecb)
;; (require 'ecb-autoloads)
;; (global-set-key [(f8)] 'ecb-toggle-ecb-windows) ; hide or show ECB windows
;; (global-set-key (kbd "ESC <f8>") 'ecb-activate)
;; ;; (global-set-key (kbd "<f7>") 'ecb-cycle-through-compilation-buffers) 
;; (setq ecb-tip-of-the-day nil)
;; (setq ecb-inhibit-startup-message t)
;; (setq ecb-information-buffer nil)
;; (setq ecb-layout-name "leftright-analyse")
;; (setq ecb-compile-window-width 'edit-window)
;; (setq ecb-compile-window-height 5)
;; (setq ecb-enlarged-compilation-window-max-height 0.68)
;; (setq ecb-change-layout-preserves-compwin-state t)
;; (add-hook 'ecb-activate-hook 
;;           (lambda() 
;;             (setq ecb-compilation-buffer-names
;;                   (quote (("*info*") ("*shell*") ("*grep*") ("\\*R:*[0-9]*\\*" . t) ("*Find*")
;;                           ("*Locate*") ("*help*") ("*Help*") ("*dictem buffer\\*[<0-9>]*" . t)
;;                           ("*TeX Help*") ("\\*help\\[R:*[0-9]*\\]([.a-zA-Z0-9_]+)\\*" . t) 
;;                           ("\\*gud-[.a-zA-Z0-9_]+\\*" . t) ("*Python*") ("*rope-pydoc*")))) 
;;             (setq ecb-primary-secondary-mouse-buttons 
;;                   (quote mouse-1--mouse-2))))
;; (setq ecb-auto-activate t)  
;;


;; Flymake (Python and LaTeX) 
(eval-after-load "flymake"
  '(progn
     (add-hook 'find-file-hook 'flymake-find-file-hook) ;; auto check
     (load-library "flymake-cursor") ;; display error in minor buffer  
     (global-set-key [f4] 'flymake-goto-next-error)
     (add-hook 'LaTeX-mode-hook 'flymake-mode)
     (setq flymake-gui-warnings-enabled nil)))

;; TODO: Note working
;; (when (load "flymake" t)
;;   (defun flymake-simple-tex-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;             temp-file
;;             (file-name-directory buffer-file-name))))
;;       ;;(list "pycheckers"  (list local-file))

;;       (list "chktex" (list "-q" "-v0" local-file))
;;       ))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.tex\\'" flymake-simple-tex-init)))


;; Auto completion
(eval-after-load "auto-complete-config"
  '(progn
     (ac-config-default)
     (ac-flyspell-workaround)
     (setq ac-auto-start 2) ; nil of not auto start
     ;; (ac-set-trigger-key "TAB") ; unset this if auto-start
     (setq ac-menu-height 10)
     (setq ac-delay 0.05)
     (setq ac-quick-help-delay 1.5)
     (setq ac-ignore-case 'smart)
     (setq ac-ignores (quote ("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

     (dolist (hook '(emacs-lisp-mode-hook 
                     c-mode-hook 
                     c++-mode-hook 
                     ess-mode-hook 
                     inferior-ess-mode-hook
                     python-mode-hook))
       (add-hook hook 'auto-complete-mode))
     (global-set-key (kbd "ESC <f7>") 'auto-complete-mode)))

;; Font lock
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t) ;; Highlight parentheses 

(eval-after-load "highlight-parentheses"
  '(progn
     (define-globalized-minor-mode global-highlight-parentheses-mode
       highlight-parentheses-mode (lambda () (highlight-parentheses-mode t)))
     (global-highlight-parentheses-mode t))) ;; Highlight symbols 

(eval-after-load "auto-highlight-symbol"
  '(progn 
     (dolist (hook
              '(emacs-lisp-mode-hook 
                c-mode-hook 
                c++-mode-hook 
                ess-mode-hook
                python-mode-hook)) 
       (add-hook hook 'auto-highlight-symbol-mode))))

;; parentheses mode 
(show-paren-mode t) 
(setq show-paren-style 'parentheses) ;; enable autopair insert globally 

(setq skeleton-pair t) 
(global-set-key "(" 'skeleton-pair-insert-maybe) 
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe) 
;;(global-set-key "\"" 'skeleton-pair-insert-maybe) 
;;(global-set-key "\'" 'skeleton-pair-insert-maybe)

;; Commenting 
(global-set-key "\M-3" 'comment-or-uncomment-region) ; Swedish keyboard 

;; (eval-after-load "yasnippet"
;;   '(progn 
;;      (yas/global-mode 1)
;;      ))

;; Add extra info path 
(eval-after-load "info-look"
  '(progn 
     (add-to-list
      'Info-default-directory-list "~/.emacs.d/info")))


;; Use ? to goto matched parenthesis 
(global-set-key "?" 'goto-match-paren) ;;
(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" 
  ;; positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t (self-insert-command (or arg 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling Check & dictionary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spelling Check ;; Spell checking using hunspell 
(eval-after-load "ispell"
  '(progn
     (setq-default ispell-program-name "hunspell") 
     (setq ispell-really-hunspell t) 
     (setq ispell-personal-dictionary "~/.hunspell") 
     (setq ispell-extra-args '("-d" "en_US"))
     (defun ispell-get-coding-system () 'utf-8) 
     (global-set-key (kbd "<f9> 4") 'ispell-word)))

;; Use M-m to auto correct words
(global-set-key (kbd "<f9> c") 'flyspell-auto-correct-word)

;; Performance
;; (setq flyspell-issue-message-flag nil) 

;; Fly spell mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode))))

;; Disable flyspell for special modes
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Fly spell check comments for a programmer
(dolist (hook '(emacs-lisp-mode-hook c-mode-hook c++-mode-hook ess-mode-hook python-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode))))

;; Dictionary
(eval-after-load "dictem"
  '(progn
     (setq dictem-server "localhost")
     (setq dictem-port   "2628")
     (dictem-initialize)
     (global-set-key (kbd "<f50>") 'dictem-run-search)
     (fset 'my-search-dictem
           [f50 return return return])
     (global-set-key (kbd "<f9> d") 'my-search-dictem)
     (setq dictem-default-database "Collins")
     (setq dictem-default-strategy "exact")
     (add-hook 'dictem-postprocess-match-hook
               'dictem-postprocess-match)
     (add-hook 'dictem-postprocess-definition-hook 
               'dictem-postprocess-definition-separator)
     (add-hook 'dictem-postprocess-definition-hook 
               'dictem-postprocess-definition-hyperlinks)
     (add-hook 'dictem-postprocess-show-info-hook
               'dictem-postprocess-definition-hyperlinks)
     (add-hook 'dictem-postprocess-definition-hook
               'dictem-postprocess-definition-remove-header)
     (define-key dictem-mode-map [tab] 'dictem-next-link)))
;;(define-key dictem-mode-map [(backtab)] 'dictem-previous-link)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/workspace/web/org/"
         :base-extension "org"
         :publishing-directory "~/workspace/web/public_html/"
         :publishing-function org-publish-org-to-html
         :recursive t
         :section-numbers nil
         :table-of-contents nil
         :auto-preamble t
         :link-up  "../"
         ;; :author-info t
         ;; :author "Feng Li"
         ;; :email "feng.li@stat.su.se"
         ;; :email-info t
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title "Sitemap"
         )
        
        ("org-notes-jekyll"
         :base-directory "~/workspace/web/org/"
         :base-extension "org"
         :publishing-directory "~/workspace/web/feng-li.github.com/_posts"
         :publishing-function org-publish-org-to-html
         :recursive t
         :headline-levels 4 
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         :section-numbers nil
         :table-of-contents nil
         )
        
        ("org-static"
         :base-directory "~/workspace/web/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|sas\\|xls"
         :publishing-directory "~/workspace/web/public_html/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("org-static-jekyll"
         :base-directory "~/workspace/web/org/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|sas\\|xls"
         :publishing-directory "~/workspace/web/feng-li.github.com/"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("website-html" 
         :components ("org-notes" "org-static")
         )
        ("website-jekyll" 
         :components ("org-notes-jekyll" "org-static-jekyll")
         )

        
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load auctex 11.86
(load "auctex.el" nil t t)
(eval-after-load "auctex.el"
  '(progn
     (load "preview-latex.el" nil t t)

     ;;LaTex AUCTex features
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (setq LaTeX-math-menu-unicode t)

     ;; Translate key § to ` so both can be used as a math abbreviation
     ;; Drawback, could mot type § anymore. Make it locally?  
     (keyboard-translate ?§ ?`) 
     (setq LaTeX-math-abbrev-prefix "`")

     (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
     (setq TeX-source-correlate-start-server t)
     (setq TeX-source-correlate-mode  t) ; for 11.86
     (setq TeX-source-correlate-method (quote source-specials)) 
     (setq bibtex-maintain-sorted-entries t)


     (require 'ac-math)
     ;; make auto-complete aware of {{{latex-mode}}}
     (add-to-list 'ac-modes 'latex-mode)   

     ;; add ac-sources to default ac-sources
     (defun ac-latex-mode-setup ()         
       (setq ac-sources
             (append '(ac-source-math-unicode 
                       ac-source-math-latex 
                       ac-source-latex-commands)
                     ac-sources)))
     (add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)

     ;; Add short cuts, hold Windows key 
     (defun auctex-insert-special ()
       (local-set-key (kbd "<f9> (") (lambda () (interactive) (insert "\\\left( ")))
       (local-set-key (kbd "<f9> )") (lambda () (interactive) (insert "\\\ right)")))
       
       (local-set-key (kbd "<f9> [") (lambda () (interactive) (insert "\\\left[ ")))
       (local-set-key (kbd "<f9> ]") (lambda () (interactive) (insert "\\\ right]")))
       
       (local-set-key (kbd "<f9> {") (lambda () (interactive) (insert "\\\left\\\{ ")))
       (local-set-key (kbd "<f9> }") (lambda () (interactive) (insert "\\\right\\\}")))
       
       (local-set-key (kbd "<f9> |") (lambda () (interactive) (insert "\\\left| \\\ right|")))
       
       (fset 'my-insert-latex-equation
             [?\\ ?\[ ?\\ ?\] left left return return up])
       (local-set-key (kbd "<f9> \\") 'my-insert-latex-equation)
       
       ;; (local-set-key (kbd "s-\\") (lambda () (interactive) (insert "\\\[\n \n\\\]")))
       
       (fset 'my-insert-bold-math
             [?\C-w ?\\ ?b ?m ?\{ ?\C-y right])
       (local-set-key (kbd "C-c C-x C-b") 'my-insert-bold-math))
     (add-hook 'LaTeX-mode-hook 'auctex-insert-special)
     (setq reftex-plug-into-AUCTeX t)

     ;; TeX view program
     ;; (setq TeX-view-program-selection (quote ((output-dvi "xdvi") (output-pdf "evince") )))
     (add-hook 'LaTeX-mode-hook (lambda () 
                                  (TeX-fold-mode 1)))
     (setq TeX-save-query  nil )
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

     (add-hook 
      'LaTeX-mode-hook
      '(lambda ()
         (local-set-key "\C-c§" 'TeX-next-error)))



     (add-hook 'LaTeX-mode-hook (lambda()
                                  (add-to-list 'TeX-command-list
                                               '("Encrypt-PDF" "pdftk \"%s.pdf\" output \"%s.SEC.pdf\" allow Printing owner_pw \"q13JCdG20yDTZr\"; mv \"%s.SEC.pdf\" \"%s.pdf\"" TeX-run-command nil nil))
                                  (add-to-list 'TeX-command-list
                                               '("Embed-Fonts-to-PDF" "gs -dSAFER -dNOPLATFONTS -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -dPDFSETTINGS=/prepress -dMaxSubsetPct=100 -dSubsetFonts=true -dEmbedAllFonts=true -sOutputFile=\"%s.embed.pdf\" -f  \"%s.pdf\";  mv \"%s.embed.pdf\" \"%s.pdf\" " TeX-run-command nil nil))
                                  (add-to-list 'TeX-command-list
                                               '("TeX2LyX" "tex2lyx -f \"%s.tex\" \"../%s.lyx\" " TeX-run-command nil nil))
                                  (add-to-list 'TeX-command-list
                                               '("LaTeXmk" "latexmk -pvc \"%s.tex\"" TeX-run-command nil nil))
                                  (add-to-list 'TeX-command-list
                                               '("PdfLaTeXmk" "latexmk -pvc -pdf \"%s.tex\"" TeX-run-command nil nil))
                                  
                                  (add-to-list 'TeX-command-list
                                               '("LaTex-DVI-PS-PDF-Adobe" "latex \"%s.tex\"; dvips \"%s.dvi\" -o \"%s.ps\"; ps2pdf \"%s.ps\"; acroread \"%s.pdf\"" TeX-run-command nil nil)) 
                                  (add-to-list 'TeX-command-list
                                               '("XeLaTeX-PDF-Adobe" "xelatex \"%s.tex\"; acroread \"%s.pdf\"" TeX-run-command nil nil))
                                  (add-to-list 'TeX-command-list
                                               '("LaTeX-DVIPDFMx-PDF-Adobe" "dvipdfmx %d; acroread \"%s.pdf\"" TeX-run-command nil nil))
                                  (add-to-list 'TeX-command-list
                                               '("View-PDF-via-Adobe" "acroread \"%s.pdf\"" TeX-run-command nil nil))))

     ;; delete the part before the cursor to beginning of the line and jump to end
     ;; of previous line. 
     ;; (fset 'my-smart-backspace
     ;;       "\C-@\C-p\C-e\C-w ")
     ;; (global-set-key [S-backspace] 'my-smart-backspace)



     (defun my-LaTeX-mode-hook ()
       "Key definitions for LaTeX mode."
       (define-key LaTeX-mode-map [(f5)] 'latex-or-view)) ;;F5 works for all
     (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)
     (fset 'latex-or-view [?\C-c ?\C-c]))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ESS (Emacs speaks statistics) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ESS
(eval-after-load "ess-site"
  '(progn
     ;; auto complete for R

     (setq ess-use-auto-complete t)
     ;; (setq ess-use-auto-complete 'script-only)

     (require 'ess-rutils)
     ;; (require 'ess-tracebug) ;; ESS tracebug
     (require 'ess-eldoc)
     (require 'r-autoyas)
     (require 'ess-R-object-tooltip)

     ;; R args at start up
     (global-set-key (kbd "ESC <f6>") 'R) ;; The default R
     (global-set-key (kbd "<f6>") 'R-2.14.1) ;; The optimize R
     (global-set-key (kbd "<f9> r") 'ess-switch-to-end-of-ESS)
     (setq-default inferior-R-args "--no-save --no-restore-data -q")


     ;; Let ESS Sweave work with AUCTEX
     (setq ess-swv-plug-into-AUCTeX-p t)

     ;; Let evaluation not viability to nil, cause emacs hang
     (setq ess-eval-visibly-p t)

     ;; Cat "\n" after evaluation R code.
     ;; (defun inferior-ess-output-filter (proc string)
     ;;   (let ((pbuf (process-buffer proc))
     ;;         (pmark (process-mark proc))
     ;;         (prompt-regexp "^>\\( [>+]\\)*\\( \\)$")
     ;;         (prompt-replace-regexp "^>\\( [>+]\\)*\\( \\)[^>+\n]"))
     ;;     (setq string (replace-regexp-in-string prompt-replace-regexp " \n"
     ;;                                            string nil nil 2))
     ;;     (with-current-buffer pbuf
     ;;       (goto-char pmark)
     ;;       (beginning-of-line)
     ;;       (when (looking-at prompt-regexp)
     ;;         (goto-char pmark)
     ;;         (insert "\n")
     ;;         (set-marker pmark (point)))))
     ;;   (comint-output-filter proc (inferior-ess-strip-ctrl-g string)))

     ;;ESS key binding
     (setq ess-ask-for-ess-directory nil)

     ;; R history files and size
     (setq ess-history-file "~/.Rhistory")

     ;; Let help on new frame
     (setq ess-help-own-frame 'one)
     (add-hook 'ess-mode-hook
               '(lambda ()                  
                  ;; ESS expression offset
                  (setq ess-expression-offset 8)  
                  
                  ;; (require 'r-autoyas)
                  ;; (define-key ess-mode-map (kbd "C-M-<tab>")
                  ;;   '(lambda ()(interactive)
                  ;;      (r-autoyas-expand nil nil)))
                  
                  ;; ESS tooltip (C-i)
                  (when window-system
                    (keyboard-translate ?\C-i ?\H-i)
                    (define-key ess-mode-map (kbd "H-i") 'ess-R-object-tooltip)
                    (define-key inferior-ess-mode-map (kbd "H-i") 'ess-R-object-tooltip))

                  
                  ;;Roxygen template
                  (setq ess-roxy-template-alist
                        (list
                         (cons "description" "<description>")
                         (cons "details" "<details>")
                         (cons "title" "<short tile>")
                         (cons "param" "")
                         (cons "return" "")
                         (cons "references" "")
                         (cons "author" "Feng Li, Department of Statistics, Stockholm University, Sweden.")
                         (cons "note" "Created: ; Current: .")))

                  (font-lock-add-keywords nil
                                          '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                            ("\\<\\(DEPENDS\\):" 1 font-lock-warning-face t)
                                            ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                                            ("\\<\\(DATE\\):" 1 font-lock-warning-face t)
                                            ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
                                            ("\\<\\(DEBUG\\):" 1 font-lock-warning-face t)
                                            ;;output values high light at comments  
                                            ("\\(\\\\item[ \t]+{[\._A-Za-z0-9]+}\\)" 1 font-lock-warning-face t) 
                                            ("\\<\\([\._A-Za-z0-9]+\$[\-\._A-Za-z0-9]+\\):" 1 font-lock-warning-face t)))

                  
                  ;; Set M-§ to complete the object in the ESS editor
                  ;; The default was "C-c Tab", Not needed if ac-R enabled
                  
                  ;; Hide and show mode
                  ;; (hs-minor-mode)
                  ;; (local-set-key (kbd "<C-M-insert>") 'hs-toggle-hiding)
                  
                  ;; insert 8 spaces
                  (fset 'my-R-smart-indent
                        [return ?\C-u ?8 ? ])
                  (local-set-key (kbd "<C-return>") 'my-R-smart-indent)

                  (fset 'my-R-comment-level-1
                        (lambda (&optional arg) "Insert level-1 R comment block" 
                          (interactive "p") 
                          (kmacro-exec-ring-item (quote ([21 55 57 35 return 21 51 35 return 21 55 57 35 up 32] 0 "%d")) arg)))
                  (local-set-key (kbd "<f9> 1") 'my-R-comment-level-1)
                  
                  ;; Insert three line comments level-2
                  (fset 'my-R-comment-level-2
                        [?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- return ?\C-u ?3 ?# return ?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- up ? ])
                  (local-set-key (kbd "<f9> 2") 'my-R-comment-level-2)
                  
                  ;; Smart indent
                  (make-local-variable 'adaptive-fill-regexp)
                  (setq adaptive-fill-regexp (concat ess-roxy-str adaptive-fill-regexp))
                  (make-local-variable 'adaptive-fill-first-line-regexp)
                  (setq adaptive-fill-first-line-regexp (concat ess-roxy-str
                                                                adaptive-fill-first-line-regexp))
                  
                  (make-local-variable 'paragraph-start)
                  (setq paragraph-start (concat "\\(" ess-roxy-str "\\)*" paragraph-start))
                  (make-local-variable 'paragraph-separate)
                  (setq paragraph-separate (concat "\\(" ess-roxy-str "\\)*" paragraph-separate))
                  (auto-fill-mode)
                  ))

     ;; Settings on R shell
     (add-hook 'inferior-ess-mode-hook
               '(lambda ()
                  (define-key inferior-ess-mode-map (kbd "C-c `") 'ess-parse-errors)
                  (define-key inferior-ess-mode-map (kbd "C-c d") 'ess-change-directory)
                  (define-key inferior-ess-mode-map (kbd "C-c l") 'ess-rutils-load-wkspc)))

     ;; (add-hook 'ess-post-run-hook 'ess-tracebug t) 
     ;; (define-key ess-mode-map "\M-]" 'next-error)
     ;; (define-key ess-mode-map "\M-[" 'previous-error)
     ;; (define-key inferior-ess-mode-map "\M-]" 'next-error-no-select)
     ;; (define-key inferior-ess-mode-map "\M-[" 'previous-error-no-select)
     ;; (define-key compilation-minor-mode-map [(?n)] 'next-error-no-select)
     ;; (define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select)

     ;; ESS Code styles
     (defun ess-code-style ()
       (local-set-key (kbd ",") (lambda () (interactive) (insert ", ")))
       (local-set-key (kbd "=") (lambda () (interactive) (insert " = ")))
       (local-set-key (kbd "<f9> =") (lambda () (interactive) (insert " == ")))
       (local-set-key (kbd "<f9> *") (lambda () (interactive) (insert " %*% ")))
       (local-set-key (kbd "<f9> x") (lambda () (interactive) (insert " %x% ")))
       (local-set-key (kbd "<f9> n") (lambda () (interactive) (insert " %in% "))))
     (add-hook 'ess-mode-hook 'ess-code-style)
     (add-hook 'inferior-ess-mode-hook 'ess-code-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fabian' python.el
;; (setq python-shell-interpreter "python2.7")
;; (setenv "PYTHONSTARTUP" "/home/fli/.pystartup")

;; Enter to indent in python.el
(add-hook 'python-mode-hook 
          '(lambda () 
             (setq python-python-command "python2.7")
             
             (define-key python-mode-map "\C-m" 'newline-and-indent)
             
             ;; Pythonmacs (use Python function in Elisp)
             (autoload 'pymacs-apply "pymacs") 
             (autoload 'pymacs-call "pymacs")
             (autoload 'pymacs-eval "pymacs" nil t)
             (autoload 'pymacs-exec "pymacs" nil t)
             (autoload 'pymacs-load "pymacs" nil t)
             (pymacs-load "ropemacs" "rope-")
             (setq ropemacs-enable-autoimport t)
             (setq pymacs-auto-restart t)
             
             ;; Flymake for Python
             (when (load "flymake" t)
               (defun flymake-pyflakes-init ()
                 (let* ((temp-file (flymake-init-create-temp-buffer-copy
                                    'flymake-create-temp-inplace))
                        (local-file (file-relative-name
                                     temp-file
                                     (file-name-directory buffer-file-name))))
                   (list "pycheckers"  (list local-file))))
               (add-to-list 'flymake-allowed-file-name-masks
                            '("\\.py\\'" flymake-pyflakes-init)))
             (setq python-check-command "pyflakes") ;; check by hand
             
             ;; Auto complete in buffer
             ;; (require 'ac-python) ;; using just python (faster)
             (ac-ropemacs-initialize) ;; using rope (intensive)
             (add-hook 'python-mode-hook
                       (lambda ()
                         (add-to-list 'ac-sources 'ac-source-ropemacs)))
             
             
             ;; DEBUGGING: PDB setup, note the python version
             (setq pdb-path '~/bin/pdb2.7.py
                   gud-pdb-command-name (symbol-name pdb-path))
             (defadvice pdb (before gud-query-cmdline activate)
               "Provide a better default command line when called interactively."
               (interactive
                (list (gud-query-cmdline pdb-path
                                         (file-name-nondirectory buffer-file-name)))))
             
             ;; Documentation lookup Bugfix for Python 2.7
             (info-lookup-add-help
              :mode 'python-mode
              :regexp "[[:alnum:]_]+"
              :doc-spec
              '(("(python)Index" nil "")))
             
             ;; ElDoc for Python in the minor buffer
             (add-hook 'python-mode-hook 'turn-on-eldoc-mode)
             
             
             (defun python-add-breakpoint ()
               (interactive)
               (newline-and-indent)
               (insert "import pdb; pdb.set_trace()"))
             (add-hook 'python-mode-hook 
                       '(lambda () (define-key python-mode-map (kbd "C-c C-t") 'python-add-breakpoint)))
             
             ;; Font-Lock
             (make-face 'font-lock-special-macro-face)
             (set-face-background 'font-lock-special-macro-face "magenta")
             (set-face-foreground 'font-lock-special-macro-face "white")
             
             (add-hook 'python-mode-hook 
                       (lambda () 
                         (font-lock-add-keywords nil
                                                 '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                                                   ("\\<\\(DEPENDS\\):" 1 font-lock-warning-face t)
                                                   ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                                                   ("\\<\\(DATE\\):" 1 font-lock-warning-face t)
                                                   ("\\<\\(DEBUG\\):" 1 font-lock-warning-face t)
                                                   ("\\<\\(import pdb;[\n \t]*pdb.set_trace()\\)" . 'font-lock-special-macro-face)))))
             
             ;; ;; Python history and python shell TODO: how? wait for python.el
             ;; ;; (add-hook 'inferior-python-mode-hook 
             ;; ;;           '(lambda()
             ;; ;;              (setq comint-input-ring-file-name "~/.pyhistory")))
             
             ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(bold ((t (:weight normal))))
 ;; '(comint-highlight-input ((t (:slant italic :weight bold))))
 ;; '(cursor ((t (:background "red" :foreground "red"))))
 ;; '(flyspell-duplicate ((t (:underline "red" :weight normal))))
 ;; '(flyspell-incorrect ((t (:underline "red" :weight normal))))
 ;; '(font-latex-italic-face ((t (:inherit nil :foreground "dark green" :slant italic))))
 ;; '(font-latex-math-face ((t (:foreground "navy"))))
 ;; '(font-latex-sectioning-5-face ((t (:foreground "red" :weight bold))))
 ;; '(font-latex-sedate-face ((t (:foreground "green"))))
 ;; '(font-latex-string-face ((t (:foreground "green4"))))
 ;; '(font-latex-warning-face ((t (:inherit nil :foreground "red"))))
 ;; '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "blue" :weight normal))))
 '(font-lock-comment-face ((t (:foreground "blue" :slant italic))))
 ;; '(font-lock-constant-face ((t (:foreground "red" :weight bold))))
 ;; '(font-lock-function-name-face ((t (:foreground "blue" :slant italic :weight bold))))
 ;; '(font-lock-keyword-face ((t (:foreground "magenta" :weight bold :width normal))))
 ;; '(font-lock-string-face ((t (:foreground "dark green" :weight normal))))
 ;; '(font-lock-type-face ((t (:foreground "blue" :weight bold))))
 ;; '(font-lock-variable-name-face ((t (:foreground "blue" :weight bold))))
 ;; '(font-lock-warning-face ((t (:inherit error :background "dark magenta" :foreground "white smoke" :weight normal))))
 ;; '(success ((t (:foreground "blue" :weight bold))))
 ;; '(warning ((t (:foreground "red" :weight bold))))

)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
