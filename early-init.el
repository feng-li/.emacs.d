(setq package-enable-at-startup t)
(setq package-user-dir (concat (getenv "HOME") "/.config/emacs/auto-save-list/elpa")) ;; Global

;; Environment variables
(setenv "OMP_NUM_THREADS" "1")
(setenv "LSP_USE_PLISTS"  "true")
(setenv "PATH" (concat (concat (getenv "HOME") "/.local/bin:")
                       (concat (getenv "HOME") "/.local/share/coursier/bin:")
                       (getenv "PATH")))

(provide 'early-init)
;;; early-init.el ends here
