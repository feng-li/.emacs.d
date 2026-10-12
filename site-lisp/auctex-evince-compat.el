;;; auctex-evince-compat.el --- Evince SyncTeX compatibility -*- lexical-binding: t; -*-

;; AUCTeX 14.2 expects lowercase Evince D-Bus names. Evince 49 uses
;; org.gnome.Evince. Support both without changing AUCTeX's installed files.

(require 'tex)
(require 'dbus nil t)
(require 'subr-x)

(defvar-local my-auctex-evince--app nil)
(defvar my-auctex-evince--registrations nil)

(defun my-auctex-evince-refresh-desktop-environment (&rest _ignored)
  "Refresh local desktop credentials before viewing or starting a build.
Keep an existing display that differs from the desktop session, such as an
SSH-forwarded display. Extra arguments allow use as before advice."
  (when (and (not (file-remote-p default-directory))
             (executable-find "systemctl"))
    (let (display authority)
      (dolist (line (ignore-errors
                      (process-lines "systemctl" "--user" "show-environment")))
        (cond ((string-prefix-p "DISPLAY=" line)
               (setq display (substring line (length "DISPLAY="))))
              ((string-prefix-p "XAUTHORITY=" line)
               (setq authority (substring line (length "XAUTHORITY="))))))
      (when (and display (not (string-empty-p display)) authority
                 (file-readable-p authority)
                 (or (not (getenv "DISPLAY"))
                     (string-empty-p (getenv "DISPLAY"))
                     (equal (getenv "DISPLAY") display)))
        (setenv "DISPLAY" display)
        (setenv "XAUTHORITY" authority)))))

(defun my-auctex-evince-sync-view ()
  "Forward-search with the Evince D-Bus interface for this buffer."
  (interactive)
  (my-auctex-evince-refresh-desktop-environment)
  (unless my-auctex-evince--app
    (my-auctex-evince-setup))
  (unless my-auctex-evince--app
    (user-error "No compatible Evince D-Bus service found"))
  (TeX-evince-sync-view-1 "gnome" my-auctex-evince--app))

(defun my-auctex-evince-setup ()
  "Enable Evince forward and inverse SyncTeX for this LaTeX buffer.
Preserve other selected PDF viewers. If Evince or D-Bus is unavailable,
leave the existing viewer configuration in place."
  (interactive)
  (when (and (not (file-remote-p default-directory))
             (executable-find "evince")
             (or (called-interactively-p 'interactive)
                 (equal (cadr (assq 'output-pdf TeX-view-program-selection))
                        "Evince")))
    (my-auctex-evince-refresh-desktop-environment)
    (let ((app (cond ((TeX-evince-dbus-p "gnome" "Evince" :forward) "Evince")
                     ((TeX-evince-dbus-p "gnome" "evince" :forward) "evince"))))
      (when app
        (setq-local my-auctex-evince--app app)
        (setq-local TeX-view-program-list
                    (cons '("Evince" my-auctex-evince-sync-view "evince")
                          (assoc-delete-all "Evince"
                                            (copy-tree TeX-view-program-list))))
        (setq-local TeX-view-program-selection
                    (cons '(output-pdf "Evince")
                          (assoc-delete-all 'output-pdf
                                            (copy-tree TeX-view-program-selection))))
        (TeX-source-correlate-mode 1)
        ;; AUCTeX itself registers the legacy lowercase inverse interface.
        (when (and (equal app "Evince")
                   (not (assoc app my-auctex-evince--registrations)))
          (push (cons app
                      (dbus-register-signal
                       :session nil (format "/org/gnome/%s/Window/0" app)
                       (format "org.gnome.%s.Window" app) "SyncSource"
                       #'TeX-source-correlate-sync-source))
                my-auctex-evince--registrations))))))

(provide 'auctex-evince-compat)
;;; auctex-evince-compat.el ends here
