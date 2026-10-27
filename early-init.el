;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t; -*-

(setq package-enable-at-startup t)
(defvar my-base-save-list (concat (getenv "HOME") "/.config/emacs" (number-to-string emacs-major-version)
                                  "." (car (split-string system-configuration "-")) "/"))
(setq package-user-dir (concat my-base-save-list "/elpa")) ;; Global

;; Reduce garbage collection during startup, then restore a moderate threshold.
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))))

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name
                               (concat my-base-save-list "/eln-cache")
                               user-emacs-directory)))

;; Environment variables
(setenv "TERM" "xterm-256color")
(setenv "OMP_NUM_THREADS" "1")
(setenv "LSP_USE_PLISTS"  "true")
;; PATH is inherited by child processes; Emacs searches exec-path itself.
(let ((user-bin-directories
       (mapcar (lambda (directory) (expand-file-name directory "~/"))
               '(".local/bin" ".cargo/bin" ".local/share/coursier/bin"))))
  (setenv "PATH" (concat (mapconcat #'identity user-bin-directories path-separator)
                        path-separator (getenv "PATH")))
  (setq exec-path (delete-dups (append user-bin-directories exec-path))))

(provide 'early-init)
;;; early-init.el ends here
