(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(display-time-mode nil)
 '(doc-view-continuous t)
 '(global-font-lock-mode t nil (font-lock))
 '(hl-paren-background-colors (quote ("light gray" "steel blue" "lime green" "orange1")))
 '(indicate-empty-lines nil)
 '(org-support-shift-select t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t nil (paren))
 '(text-mode-hook (quote (turn-on-auto-fill (lambda nil (flyspell-mode)) (lambda nil (turn-on-auto-fill)) text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(warning-suppress-types (quote ((undo discard-info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feng Li's .emacs configurations
;;
;; Copyright: Feng Li <http://feng.li/>
;;
;; Download: https://github.com/feng-li/.emacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(require 'iso-transl) ;; keyboard input definitions for ISO 8859/1
(require 'htmlize-view)
(require 'session)
;;(require 'ibus)
(require 'ibuffer)
(require 'ido)
(require 'comint)
(require 'org-install)
(require 'markdown-mode)
;; (require 'flymake)
(require 'dictem)
(require 'auto-complete-config)
;;(require 'highlight-parentheses)
(require 'auto-highlight-symbol)
;;(require 'yasnippet)
(require 'info-look)
(require 'ess-site)
;;(require 'matlab-load)
;;(require 'egg)
(require 'git-emacs)
(require 'git-blame)
(require 'python)
(require 'artbollocks-mode)
;;(require 'predictive)
(load "auctex.el" nil t t)
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

;; Enable line number mode and enable visual line mode
(add-hook 'find-file-hook
          (lambda ()
            (linum-mode 1)
            (visual-line-mode 1)))

;; Saving options
(fset 'single-line-only
   [?\C-x ?h ?\C-\M-\% ?^ ?\C-q ?\C-j ?\C-q ?\C-j ?+ return ?\C-q ?\C-j return])

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Let Alt key be the meta key
(setq x-alt-keysym 'meta)

;; Chinese input method ibus
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
            (define-key dired-mode-map (kbd "<delete>") 'dired-do-delete)
            (define-key dired-mode-map (kbd "<f9> DEL")
              (lambda () (interactive) (find-alternate-file "..")))
            (define-key dired-mode-map (kbd "s") 'find-in-workspace)
            (setq cursor-type 'box)
            (dired-omit-mode 1)
            (local-set-key (kbd "<f9> h") 'dired-omit-mode)))
(put 'dired-find-alternate-file 'disabled nil)

;; disable tooltips
(tooltip-mode nil)

;; Personal global key settings
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
(setq initial-scratch-message nil) ;; Disable scratch information
(setq fundamental-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq initial-major-mode 'text-mode) ;; text mode in scratch
(add-hook 'text-mode-hook
          (function (lambda () (turn-on-auto-fill)))) ;; Auto fill mode

;; Kill the current buffer, without confirmation.
(fset 'my-kill-current-buffer
      [?\C-x ?k return])
(global-set-key (kbd "<f9> k") 'my-kill-current-buffer)

;; Bind undo with the common keyboard
(global-set-key (kbd "C-z") 'undo)

;; F2 switch to previous buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "<f2>") 'switch-to-previous-buffer)

;;;set server-start
;;(server-start)

;; Default English fonts
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
;; (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10"))

;; Set Chinese fonts
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
;; Set export TERM="xterm-256color" in .bashrc and
;; term "screen-256color" in .screenrc.
(if (equal "xterm-256color" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])

  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5C" [C-right]))

(if (equal "screen-256color" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\e[1;2B" [S-down])
  (define-key input-decode-map "\e[1;2D" [S-left])
  (define-key input-decode-map "\e[1;2C" [S-right])

  (define-key input-decode-map "\e[1;5A" [C-up])
  (define-key input-decode-map "\e[1;5B" [C-down])
  (define-key input-decode-map "\e[1;5D" [C-left])
  (define-key input-decode-map "\e[1;5C" [C-right])
  (define-key global-map [select] 'end-of-buffer))

;; (if (equal "screen" (tty-type))
;;     (define-key input-decode-map "\e[1;2A" [S-up]))


;;shift selection
(setq shift-select-mode t)

;; make typing override text selection
(delete-selection-mode 1) ;

;; TAB settings
(setq-default indent-tabs-mode nil)

;; Control-tab to switch among buffers
(when window-system
  (global-set-key (kbd "C-<tab>") 'next-buffer))

;; Keep buffer order during switch
;; (require 'flobl)

;;Session(Keep section each time)
(eval-after-load "session"
  '(progn
     (add-hook 'after-init-hook 'session-initialize)))

;; Cursor is bar: Not clear under console
(setq-default cursor-type 'bar)

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

;; Ido mode
(eval-after-load "ido"
  '(progn
     (ido-mode t)
     (setq ido-use-virtual-buffers nil)
     (setq ido-enable-flex-matching t)
     (setq ido-ignore-files
           '("\\.Rc$" "\\.dvi$" "\\.pdf$" "\\.ps$" "\\.out$"
             "\\.log$" "\\.ods$" "\\.eps$" "\\#$" "\\.png$"
             "\\.RData$" "\\.nav$" "\\.snm$" "\\`\\.\\./" "\\`\\./"))
     (setq  ido-ignore-buffers
            '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
              "^\\*Ibuffer*" "^\\*ESS-errors*" "^\\*Warnings*" "
              output*" "*TeX Help*" "*grep*"
              "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
              "_region_" "^TAGS$" "^\*Ido" "^\\*.*dictem
     buffer\\*$" "^\\*inferior-lisp*")) ))

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
(defun comint-clear-buffer () (interactive)
  (save-excursion
    (comint-goto-process-mark)
    (forward-line 0)
    (kill-region (point-min) (point))))
(define-key comint-mode-map (kbd "C-l") 'comint-clear-buffer)

;; Replace ^M
(fset 'my-replace-m
      [escape ?< escape ?% ?\C-q ?\C-m return ?  return ?! escape ?<])

;; Insert Current time, linux only?
(global-set-key "\C-ct" 'my-insert-time)
(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Z %Y")))

;; Flymake (Python and LaTeX)
(eval-after-load "flymake"
  '(progn
     (add-hook 'find-file-hook 'flymake-find-file-hook) ;; auto check
     (load-library "flymake-cursor") ;; display error in minor buffer
     (global-set-key [f4] 'flymake-goto-next-error)
     (add-hook 'LaTeX-mode-hook 'flymake-mode)
     (setq flymake-gui-warnings-enabled nil)))

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
     (global-set-key (kbd "<f9> a") 'auto-complete-mode)))

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
;;(global-set-key "{" 'skeleton-pair-insert-maybe)
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


;; Goto matched parenthesis
(global-set-key (kbd "M-6") 'goto-match-paren) ;;
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
;; Spelling checking & dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Spelling Check
(eval-after-load "ispell"
  '(progn
     (setq-default ispell-program-name "hunspell")
     (setq ispell-really-hunspell t)
     (setq ispell-personal-dictionary "~/.hunspell")
     (setq ispell-extra-args '("-d" "en_US"))
     (defun ispell-get-coding-system () 'utf-8)
     (global-set-key (kbd "<f9> 4") 'ispell-word)))

;; Auto correct spelling mistakes
(global-set-key (kbd "<f9> c") 'flyspell-auto-correct-word)

;; Fly spell performance
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
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "markdown-mode"
  '(progn
     (setq auto-mode-alist
           (cons '("\\.md" . markdown-mode) auto-mode-alist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "org-install"
  '(progn
     ;; (require 'org)
     ;; (require 'org-html)
     ;; (require 'htmlize)
     (setq org-export-default-language "en"
           org-export-html-extension "html"
           org-export-with-timestamps t
           org-export-with-section-numbers nil
           org-export-with-tags 'not-in-toc
           org-export-skip-text-before-1st-heading nil
           org-export-with-sub-superscripts '{}
           org-export-with-LaTeX-fragments t
           org-export-with-archived-trees nil
           org-export-highlight-first-table-line t
           org-export-latex-listings-w-names nil
           org-export-html-style-include-default nil
           org-export-htmlize-output-type 'css
           org-startup-folded nil
           org-publish-list-skipped-files t
           org-publish-use-timestamps-flag t
           org-export-babel-evaluate nil
           org-confirm-babel-evaluate nil)

     (setq org-publish-project-alist
           '(("org-web"
              :publishing-function org-publish-org-to-html
              :base-directory "~/workspace/web/org/"
              :publishing-directory "~/workspace/web/html/"
              :base-extension "org"
              :html-extension "html"
              :recursive t
              :section-numbers nil
              :table-of-contents nil
              ;;  :html-preamble ,(org-get-file-contents "~/workspace/web/html/style/preamble.html")
              ;; :html-postamble ,(org-get-file-contents "~/workspace/web/html/style/postamble.html")

              ;; :style ,(org-get-file-contents "~/workspace/web/html/style/stylesheet.html")
              :auto-sitemap t
              :sitemap-filename "sitemap.org"
              :sitemap-title "Sitemap"
              )

             ;; ("org-notes-jekyll"
             ;;  :base-directory "~/workspace/web/org/"
             ;;  :base-extension "org"
             ;;  :publishing-directory "~/workspace/web/feng-li.github.com/_posts"
             ;;  :publishing-function org-publish-org-to-html
             ;;  :recursive t
             ;;  :headline-levels 4
             ;;  :html-extension "html"
             ;;  :body-only t ;; Only export section between <body> </body>
             ;;  :section-numbers nil
             ;;  :table-of-contents nil
             ;;  )

             ;; ("org-static"
             ;;  :base-directory "~/workspace/web/org/"
             ;;  :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|sas\\|xls"
             ;;  :publishing-directory "~/workspace/web/public_html/"
             ;;  :recursive t
             ;;  :publishing-function org-publish-attachment
             ;;  )

             ;; ("org-static-jekyll"
             ;;  :base-directory "~/workspace/web/org/"
             ;;  :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|sas\\|xls"
             ;;  :publishing-directory "~/workspace/web/feng-li.github.com/"
             ;;  :recursive t
             ;;  :publishing-function org-publish-attachment
             ;;  )

             ;; ("website-html"
             ;;  :components ("org-notes" "org-static")
             ;;  )
             ;; ("website-jekyll"
             ;;  :components ("org-notes-jekyll" "org-static-jekyll")
             ;;  )


             ))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

     ;; add ac-sources to default ac-sources in LaTeX mode.
     (defun ac-latex-mode-setup ()
       (setq ac-sources
             (append '(ac-source-math-unicode
                       ac-source-math-latex
                       ac-source-latex-commands
                       ac-source-words-in-same-mode-buffers)
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


     (setq LaTeX-command-style (quote (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))

     ;; TeX view program
     (setq TeX-output-view-style
           (quote
            (("^pdf$" "." "evince -f %o")
             )))

     ;; (setq TeX-view-program-selection (quote ((output-dvi "xdvi") (output-pdf "evince") )))
     (add-hook 'LaTeX-mode-hook (lambda ()
                                  (TeX-fold-mode 1)))
     (setq TeX-save-query  nil )
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

     (add-hook
      'LaTeX-mode-hook
      '(lambda ()
         (local-set-key (kbd "C-c `") 'TeX-next-error)))

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

     (defun ac-ess-mode-setup ()
       (setq ac-sources
             (append '(ac-source-words-in-same-mode-buffers)
                     ac-sources)))
     (add-hook 'ess-mode-hook 'ac-ess-mode-setup)
     (add-hook 'inferior-ess-mode-hook 'ac-ess-mode-setup)

     ;; (setq ess-use-auto-complete 'script-only)

     (require 'ess-rutils)
     ;; (require 'ess-tracebug) ;; ESS tracebug
     (require 'ess-eldoc)
     ;; (require 'ess-R-object-tooltip)

     ;; R args at start up
     (global-set-key (kbd "<f9> <f6>") 'R) ;; The default R
     (global-set-key (kbd "<f9> r") 'ess-switch-to-end-of-ESS)
     (setq-default inferior-R-args "--no-save --no-restore-data -q")

     ;; Let ESS Sweave work with AUCTEX
     (setq ess-swv-plug-into-AUCTeX-p t)

     ;; Let evaluation not viability to nil, cause emacs hang
     (setq ess-eval-visibly-p t)

     ;;ESS key binding
     (setq ess-ask-for-ess-directory nil)

     ;; R history files and size
     (setq ess-history-file "~/.Rhistory")

     ;; Let help on new frame
     (setq ess-help-own-frame 'one)
     (add-hook 'ess-mode-hook
               '(lambda ()
                  ;; ESS expression offset
                  (setq ess-arg-function-offset-new-line '(4))

                  ;; (require 'r-autoyas)
                  ;; (define-key ess-mode-map (kbd "C-M-<tab>")
                  ;;   '(lambda ()(interactive)
                  ;;      (r-autoyas-expand nil nil)))

                  ;; ESS tooltip (C-i)
                  ;; (when window-system
                  ;;   (keyboard-translate ?\C-i ?\H-i)
                  ;;   (define-key ess-mode-map (kbd "H-i") 'ess-R-object-tooltip)
                  ;;   (define-key inferior-ess-mode-map (kbd "H-i") 'ess-R-object-tooltip))


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


(eval-after-load "python"
  '(progn

     ;; Fabian' python.el
     (setq python-shell-interpreter "python2.7")
     (setenv "PYTHONSTARTUP" "/home/fli/.pystartup")

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
                                                        ("\\<\\(import pdb;[\n \t]*pdb.set_trace()\\)" .
                                                         'font-lock-special-macro-face)))))

                  ;; ;; Python history and python shell TODO: how? wait for python.el
                  ;; ;; (add-hook 'inferior-python-mode-hook
                  ;; ;;           '(lambda()
                  ;; ;;              (setq comint-input-ring-file-name "~/.pyhistory")))

                  ))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-function-call-face ((t (:inherit font-lock-builtin-face :slant normal :weight bold :width normal))))
 '(flyspell-duplicate ((t (:underline "red" :weight normal))))
 '(flyspell-incorrect ((t (:underline "red" :weight normal))))
 '(font-latex-italic-face ((t (:slant italic))))
 '(font-lock-builtin-face ((t (:foreground "darkcyan"))))
 '(font-lock-comment-face ((t (:foreground "blue" :slant italic))))
 '(font-lock-function-name-face ((t (:foreground "darkcyan" :slant italic :weight bold))))
 '(font-lock-string-face ((t (:foreground "darkgreen"))))
 '(match ((t (:background "yellow1" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "magenta")))))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
