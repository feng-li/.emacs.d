(setq package-enable-at-startup t)
(setq package-user-dir (concat (getenv "HOME") "/.config/emacs/elpa")) ;; Global

;; Set eln-cache dir
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name
                               (concat (getenv "HOME") "/.config/emacs/eln-cache")
                               user-emacs-directory)))

;; Environment variables
(setenv "OMP_NUM_THREADS" "1")
(setenv "LSP_USE_PLISTS"  "true")
(setenv "PATH" (concat (concat (getenv "HOME") "/.local/bin:")
                       (concat (getenv "HOME") "/.cargo/bin:")
                       (concat (getenv "HOME") "/.local/share/coursier/bin:")
                       (getenv "PATH")))

(provide 'early-init)
;;; early-init.el ends here
