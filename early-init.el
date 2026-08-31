;;; early-init.el --- Early Emacs initialization -*- lexical-binding: t; -*-

(setq package-enable-at-startup t)
(setq package-user-dir (concat (getenv "HOME") "/.config/emacs" (number-to-string emacs-major-version)  "/elpa")) ;; Global

;; Reduce garbage collection during startup, then restore a moderate threshold.
(setq gc-cons-threshold (* 1024 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))))

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name
                               (concat (getenv "HOME") "/.config/emacs" (number-to-string emacs-major-version) "/eln-cache")
                               user-emacs-directory)))

;; Environment variables
(setenv "TERM" "xterm-256color")
(setenv "OMP_NUM_THREADS" "1")
(setenv "LSP_USE_PLISTS"  "true")
(setenv "PATH" (concat (concat (getenv "HOME") "/.local/bin:")
                       (concat (getenv "HOME") "/.cargo/bin:")
                       (concat (getenv "HOME") "/.local/share/coursier/bin:")
                       (getenv "PATH")))

(provide 'early-init)
;;; early-init.el ends here
