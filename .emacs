(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(column-number-mode t)
 '(doc-view-continuous t)
 '(frame-background-mode (quote dark))
 '(global-font-lock-mode t nil (font-lock))
 '(hl-paren-background-colors (quote ("light gray" "steel blue" "lime green" "orange1")))
 '(indicate-empty-lines nil)
 '(org-support-shift-select t)
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t nil (paren))
 '(warning-suppress-types (quote ((undo discard-info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Feng Li's .emacs configurations
;;
;; Copyright: Feng Li <http://feng.li/>
;;
;; Download: https://github.com/feng-li/.emacs.d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load all required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Add personal load path recursively in front of the default load path
(let ((default-directory "~/.emacs.d/site-lisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")

;; Byte compile directory when files are changed
;; (setq byte-compile-warnings nil)
;; (byte-recompile-directory (expand-file-name "~/.emacs.d/site-lisp/") 0)

;; Additional library loaded during start up.
(require 'iso-transl) ;; keyboard input definitions for ISO 8859/1
(require 'session)
(require 'ibuffer)
(require 'ido)
(require 'comint)
(require 'org)
(require 'markdown-mode)
;; (require 'flymake)
(require 'dictem nil 'noerror)
(require 'auto-complete-config)
(require 'info-look)
(require 'ess-site)
(require 'python)
(load "auctex.el" nil t t)
(require 'langtool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set home directory
;; (setq default-directory "~/workspace/")

;; Default frame height and width
(setq default-frame-alist
      (append (list '(width  . 95)  ; Width set (characters)
                    '(height . 55)) ; Height set (lines)
              default-frame-alist))
;; set the default fill column
(setq default-fill-column 90)

;; Personal information
(setq frame-title-format "%b")
(setq user-full-name "Feng Li")
(setq user-mail-address "m@feng.li")

;; Desktop save mode
;; (desktop-save-mode 1)

;; Environment variables
(setenv "OMP_NUM_THREADS" "1")

(setq explicit-bash-args '("--init-file" "~/.bashrc"))


;; Theme
(load-theme 'solarized t)


(when (display-graphic-p)

  ;; tool-bar mode
  (tool-bar-mode -1)

  ;; Disable scroll-bar
  (scroll-bar-mode -1)
  )

;; Disable backup files (*~)
(setq make-backup-files nil)

;; Disable vc-git
(setq vc-handled-backends ())
(vc-mode -1)

;; Set Fonts
(when (display-graphic-p)
  (add-to-list 'default-frame-alist
               '(font . "Droid Sans Mono-10.5"))
  (setq face-font-rescale-alist
        '(("Microsoft YaHei". 1.2)))
  (set-fontset-font "fontset-default"
                    'unicode '("Microsoft YaHei" . "unicode-bmp"))
  )
;; Menu bar
(menu-bar-mode t)

;; Tooltip mode
(tooltip-mode nil)

;; Control-tab to switch among buffers
(global-set-key (kbd "C-<tab>") 'next-buffer)

;; Global auto revert mode
;; (global-auto-revert-mode t)

;; Key bind to increase and decrease text size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Speed bar
;; (speedbar t)
;; (speedbar-add-supported-extension (quote(".R" ".r" ".bib" ".org")))

;; )


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


;; Follow mode (dual pages display)
(global-set-key (kbd "C-<f2>")  'follow-delete-other-windows-and-split)


;; Suspend and resume hook
(add-hook 'suspend-hook
          (function (lambda ()
                      (or (y-or-n-p
                           "Really suspend emacs?")
                          (error "Suspend canceled.")))))
(add-hook 'suspend-resume-hook
          (function (lambda () (message "Emacs resumed!"))))


;;Mutt
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))



;; Global visual line mode
(global-visual-line-mode -1)

;; Dired mode
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq dired-omit-files-p t)
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
            (setq cursor-type 'box)
            (dired-omit-mode 1)
            (local-set-key (kbd "<f9> h") 'dired-omit-mode)))
(put 'dired-find-alternate-file 'disabled nil)

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

;; Switch to previous buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "ESC <f2>") 'switch-to-previous-buffer)

;; set server-start
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start)
  )
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))
(add-hook 'server-done-hook 'delete-frame)

;;stop start up message
(setq inhibit-startup-message t)

;;Use y-n short
(fset 'yes-or-no-p 'y-or-n-p)

;; Allow shift-arrow keys and control-arrow keys under different tty
;; Set export TERM="xterm" in .bashrc and
;; term "xterm" in .screenrc.

(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

;;shift selection
(setq shift-select-mode t)

;; allow mouse to select
(setq xterm-mouse-mode t)

;; make typing override text selection
(delete-selection-mode 1) ;

;; TAB settings
(setq-default indent-tabs-mode nil)


;;Session(Keep section each time)
(eval-after-load "session"
  '(progn
     (setq session-use-package nil)
     (add-hook 'after-init-hook 'session-initialize)))

;; Cursor is bar: Not clear under console
(setq-default cursor-type 'box)

;;set visible-bell
(setq visible-bell t)

;; set big kill ring
(setq kill-ring-max 150)

;; auto fill mode
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               'org-mode-hook
	       'mail-mode-hook
               'ess-mode-hook))
  (add-hook hook '(lambda () (auto-fill-mode 1))))


;; copy with other applications
(setq x-select-enable-clipboard t)

(setq ring-bell-function (lambda ()  t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diary mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq diary-file "~/workspace/diary")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab, Octave mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Let m-file connected with octave mode.
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LanguageTool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "langtool"
  '(progn
     (setq langtool-language-tool-jar "~/.APP/LanguageTool/languagetool-commandline.jar")
     (setq langtool-default-language "en-US")


     (global-set-key "\C-x4w" 'langtool-check)
     (global-set-key "\C-x4W" 'langtool-check-done)
     (global-set-key "\C-x4l" 'langtool-switch-default-language)
     (global-set-key "\C-x44" 'langtool-show-message-at-point)
     (global-set-key "\C-x4c" 'langtool-correct-buffer)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General IDE settings (ElDoc, ECB, Comint...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAGS
(setq tags-file-name "~/code/TAGS")
;; (setq tags-table-list
;;       '("~/.emacs.d/tags" "~/code/"))
;; (visit-tags-table-buffer t)

;; Ibuffer mode

(eval-after-load "ibuffer"
  '(progn
     (global-set-key (kbd "C-x C-b") 'ibuffer)
     (global-set-key (kbd "<f9> i") 'ibuffer)

     (setq ibuffer-saved-filter-groups
           (quote
            (("default"
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
             "\\.log$" "\\.ods$" "\\.eps$" "\\#$" "\\.png$" "\\~$"
             "\\.RData$" "\\.nav$" "\\.snm$" "\\`\\.\\./" "\\`\\./"
             "\\.synctex.gz$" "\\.fdb_latexmk$"))

     (setq  ido-ignore-directories
            '("\\.prv"))

     (setq ido-ignore-buffers
           '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*"
             "^\\*Buffer" "^\\*Ibuffer*" "^\\*ESS-errors*"
             "^\\*Warnings*" "output*" "*TeX Help*"
             "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
             "_region_" "^TAGS$" "^\*Ido" "^\\*.*dictem buffer\\*$"
             "^\\*inferior-lisp*" "^\\*Compile-Log\\*"))
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


;; Auto complete mode
(eval-after-load "auto-complete-config"
  '(progn
     (ac-config-default)
     (ac-flyspell-workaround) ; prevent AutoComplete striking as soon as I enable Flymake
     (ac-linum-workaround)
     (setq ac-auto-start 2) ; nil of not auto start
     ;; (ac-set-trigger-key "TAB") ; unset this if auto-start
     (setq ac-menu-height 10)
     (setq ac-use-comphist nil) ;slow when exit Emacs
     (define-key ac-completing-map [tab] 'ac-complete)
     (define-key ac-completing-map [return] nil)
     (setq ac-delay 0.05)
     (setq ac-quick-help-delay 1.5)
     (setq ac-ignore-case 'smart)
     ))

(dolist (hook '(emacs-lisp-mode-hook
                c-mode-hook
                c++-mode-hook
                ess-mode-hook
                org-mode-hook
                inferior-ess-mode-hook
                python-mode-hook))
  (add-hook hook 'auto-complete-mode))
(global-set-key (kbd "<f9> a") 'auto-complete-mode)

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
(setq show-paren-style 'parentheses) ;; enable autopair insert globally
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
;;(global-set-key "{" 'skeleton-pair-insert-maybe)
;;(global-set-key "\"" 'skeleton-pair-insert-maybe)
;;(global-set-key "\'" 'skeleton-pair-insert-maybe)

;; Commenting
(global-set-key (kbd "M-3") 'comment-or-uncomment-region)


;; Add extra info path
(eval-after-load "info-look"
  '(progn
     (add-to-list
      'Info-default-directory-list "~/.emacs.d/info")))


;; Goto matched parenthesis
(global-set-key (kbd "?") 'goto-match-paren) ;;
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
     (setq ispell-program-name (executable-find "hunspell"))
     (setq ispell-really-hunspell t)
     (setq ispell-personal-dictionary "~/.emacs.d/hunspell/")
     (add-to-list 'ispell-local-dictionary-alist
                  '("english-hunspell" "[[:alpha:]]" "[^[:alpha:]]" "[']"
                    t ("-d" "en_US") nil utf-8))

     (defun ispell-get-coding-system () 'utf-8)
     (global-set-key (kbd "<f9> 4") 'ispell-word)))

;; Auto correct spelling mistakes
(global-set-key (kbd "<f9> c") 'flyspell-auto-correct-word)

;; Fly spell performance
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-issue-message-flag nil)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "markdown-mode"
  '(progn
     (setq auto-mode-alist
           (cons '("\\.md" . markdown-mode) auto-mode-alist))))

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
     (load "preview-latex.el" nil t t)

     ;; LaTeX AUCTex features
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (setq LaTeX-math-menu-unicode t)

     ;; Translate key § to ` so both can be used as a math abbreviation
     ;; Drawback, could not type § anymore. Make it locally?
     (keyboard-translate ?§ ?`)
     (setq LaTeX-math-abbrev-prefix "`")

     (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

     (setq TeX-source-correlate-mode  t)
     (setq TeX-source-correlate-start-server nil)

     ;; Set default TeX engine
     (setq TeX-PDF-mode t)
     ;; (setq-default TeX-engine 'xetex) ;this can be set locally

     (setq TeX-parse-self t) ; Enable parse on load.
     (setq TeX-auto-save t) ; Enable parse on save.


     ;; Add listings to verbatim environments
     (eval-after-load 'latex
       '(add-to-list 'LaTeX-verbatim-environments "lstlisting"))


     (setq TeX-source-correlate-method (quote source-specials)) ; only for dvi
     (setq TeX-source-correlate-method (quote synctex)) ;only for evince
     (setq bibtex-maintain-sorted-entries t)

     ;; Add short cuts, hold Windows key
     (defun auctex-insert-special ()
       (local-set-key (kbd "<f9> (") (lambda () (interactive) (insert "\\\left( ")))
       (local-set-key (kbd "<f9> )") (lambda () (interactive) (insert "\\\ right)")))

       (local-set-key (kbd "<f9> [") (lambda () (interactive) (insert "\\\left[ ")))
       (local-set-key (kbd "<f9> ]") (lambda () (interactive) (insert "\\\ right]")))

       (local-set-key (kbd "<f9> {") (lambda () (interactive) (insert "\\\left\\\{ ")))
       (local-set-key (kbd "<f9> }") (lambda () (interactive) (insert "\\\right\\\}")))

       (local-set-key (kbd "<f9> |") (lambda () (interactive) (insert "\\\left| \\\ right|")))

       (local-set-key (kbd "<f9> |") (lambda () (interactive) (insert "\\\left| \\\ right|")))


       (local-set-key (kbd "C-\\") (lambda () (interactive) (insert "\\")))

       ;; Use \bm{} to repace \mathbf{}
       (fset 'my-insert-bold-math
             [?\C-w ?\\ ?b ?m ?\{ ?\C-y ?\} right])
       (local-set-key (kbd "C-c C-x C-b") 'my-insert-bold-math))
     (add-hook 'LaTeX-mode-hook 'auctex-insert-special)

     ;; Enable file-line-error to avoid error message "Error occured after last TeX file closed"
     (setq LaTeX-command-style (quote (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))


     (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
     (setq TeX-save-query  nil )

     ;; RefTeX
     (setq reftex-plug-into-AUCTeX t)
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (setq reftex-cite-format 'natbib)
     (setq reftex-use-external-file-finders t)
     (setq reftex-external-file-finders
           '(("tex" . "kpsewhich -format=.tex %f")
             ("bib" . "kpsewhich -format=.bib %f")
	     ("bst" . "kpsewhich -format=.bst %f")))

     ;; LaTeX Command list
     (add-hook 'LaTeX-mode-hook
               '(lambda ()
                  (local-set-key (kbd "C-c `") 'TeX-next-error)
                  (local-set-key (kbd "<f5>") 'TeX-command-run-all)
                  ))

     (add-hook
      'LaTeX-mode-hook
      (lambda()
        (add-to-list 'TeX-command-list
                     '("TeX2LyX" "tex2lyx -f %s.tex ../%s.lyx "
                       TeX-run-command nil (latex-mode)))
        (add-to-list 'TeX-command-list
                     '("View-PDF-via-Adobe" "acroread %s.pdf"
                       TeX-run-command nil (latex-mode)))
        ))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ESS (Emacs speaks statistics)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

     ;; ESS tracebug
     (setq ess-use-tracebug nil)

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
     (setq ess-eval-visibly-p nil)

     ;;ESS key binding
     (setq ess-ask-for-ess-directory nil)

     ;; R history files and size
     (setq ess-history-file "~/.Rhistory")

     ;; Let help on new frame
     ;; (setq ess-help-own-frame t)


     (add-hook 'ess-mode-hook
               '(lambda ()

                  ;;Roxygen template
                  (setq ess-roxy-template-alist
                        (list
                         (cons "description" "<title>")
                         (cons "details" "<description>")
                         (cons "param" "")
                         (cons "return" "NA")
                         (cons "references" "NA")
                         (cons "author" "Feng Li, Central University of Finance and Economics.")))

                  (font-lock-add-keywords
                   nil
                   '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                     ("\\<\\(DEPENDS\\):" 1 font-lock-warning-face t)
                     ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                     ("\\<\\(DATE\\):" 1 font-lock-warning-face t)
                     ("\\<\\(NOTE\\):" 1 font-lock-warning-face t)
                     ("\\<\\(DEBUG\\):" 1 font-lock-warning-face t)
                     ;;output values high light at comments
                     ("\\(\\\\item[ \t]+{[\._A-Za-z0-9]+}\\)" 1
                      font-lock-warning-face t)
                     ("\\<\\([\._A-Za-z0-9]+\$[\-\._A-Za-z0-9]+\\):" 1
                      font-lock-warning-face t)))


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
                          (kmacro-exec-ring-item
                           (quote ([21 55 57 35 return 21 51 35
                                       return 21 55 57 35 up 32] 0 "%d")) arg)))
                  (local-set-key (kbd "<f9> 1") 'my-R-comment-level-1)

                  ;; Insert three line comments level-2
                  (fset 'my-R-comment-level-2
                        [?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- return
                               ?\C-u ?3 ?# return ?\C-a ?\C-u ?3 ?# ?\C-u ?7 ?6 ?- up ? ])
                  (local-set-key (kbd "<f9> 2") 'my-R-comment-level-2)


                  ;; (make-local-variable 'adaptive-fill-regexp)
                  ;; (setq adaptive-fill-regexp (concat ess-roxy-str adaptive-fill-regexp))
                  ;; (make-local-variable 'adaptive-fill-first-line-regexp)
                  ;; (setq adaptive-fill-first-line-regexp
                  ;;       (concat ess-roxy-str
                  ;;               adaptive-fill-first-line-regexp))

                  ;; (make-local-variable 'paragraph-start)
                  ;; (setq paragraph-start (concat "\\(" ess-roxy-str "\\)*" paragraph-start))
                  ;; (make-local-variable 'paragraph-separate)
                  ;; (setq paragraph-separate
                  ;;       (concat "\\(" ess-roxy-str "\\)*" paragraph-separate))
                  (auto-fill-mode)
                  ))

     ;; Settings on R shell
     (add-hook 'inferior-ess-mode-hook
               '(lambda ()
                  (define-key inferior-ess-mode-map (kbd "C-c `") 'ess-parse-errors)
                  (define-key inferior-ess-mode-map (kbd "C-c d") 'ess-change-directory)
                  (define-key inferior-ess-mode-map (kbd "C-k")   'kill-whole-line)
                  (define-key inferior-ess-mode-map (kbd "C-c l") 'ess-rutils-load-wkspc)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-after-load "python"
  '(progn

     ;; Fabian' python.el (Now default in Emacs)
     (setq python-shell-interpreter "python2.7")
     ;; (setenv "PYTHONSTARTUP" "/home/fli/.pystartup")

     ;; Enter to indent in python.el
     (add-hook 'python-mode-hook
               '(lambda ()
                  (setq python-python-command "python")

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
                  (add-hook
                   'python-mode-hook
                   '(lambda ()
                      (define-key python-mode-map
                        (kbd "C-c C-t") 'python-add-breakpoint)))

                  ;; Font-Lock
                  (make-face 'font-lock-special-macro-face)
                  (set-face-background 'font-lock-special-macro-face "magenta")
                  (set-face-foreground 'font-lock-special-macro-face "white")

                  (add-hook
                   'python-mode-hook
                   (lambda ()
                     (font-lock-add-keywords
                      nil
                      '(("\\<\\(FIXME\\):" 1 font-lock-warning-face t)
                        ("\\<\\(DEPENDS\\):" 1 font-lock-warning-face t)
                        ("\\<\\(TODO\\):" 1 font-lock-warning-face t)
                        ("\\<\\(DATE\\):" 1 font-lock-warning-face t)
                        ("\\<\\(DEBUG\\):" 1 font-lock-warning-face t)
                        ("\\<\\(import pdb;[\n \t]*pdb.set_trace()\\)" .
                         'font-lock-special-macro-face)))))


                  ))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customize faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
