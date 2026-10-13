;;; auctex-evince-compat.el --- Desktop startup and Evince SyncTeX -*- lexical-binding: t; -*-

;;; Commentary:
;; Import local graphical-session credentials once during startup, or when
;; the first local graphical frame is ready.  Do not refresh them during builds
;; or forward/inverse search.
;;
;; AUCTeX 14.2 expects lowercase Evince D-Bus names. Evince 49 uses
;; org.gnome.Evince. Support both without changing AUCTeX's installed files.

;;; Code:

(require 'subr-x)
(require 'seq)

;; Load AUCTeX only when its viewer setup is used, not during desktop startup.
(defvar TeX-view-program-selection)
(defvar TeX-view-program-list)
(declare-function TeX-evince-dbus-p "tex" (de app &rest options))
(declare-function TeX-evince-sync-view-1 "tex" (de app))
(declare-function TeX-source-correlate-mode "tex" (&optional arg))
(declare-function TeX-source-correlate-sync-source "tex" (file linecol &rest ignored))
(declare-function dbus-register-signal "dbus" (bus service path interface signal handler &rest args))

(defvar my-desktop-environment--imported-p nil
  "Non-nil once this Emacs process has imported its desktop environment.")

(defconst my-desktop-environment--variables
  '("DISPLAY" "XAUTHORITY" "WAYLAND_DISPLAY" "XDG_RUNTIME_DIR"
    "DBUS_SESSION_BUS_ADDRESS"))

(defun my-desktop-environment-import-once (&optional frame)
  "Import local desktop credentials once, when the desktop is ready.
FRAME is supplied by `after-make-frame-functions'; ignore terminal and
forwarded graphical frames.  Preserve a different inherited DISPLAY or
D-Bus address rather than mixing separate sessions.  Never reconnect D-Bus."
  (when (and (not my-desktop-environment--imported-p)
             (eq system-type 'gnu/linux)
             (not (file-remote-p user-emacs-directory))
             (or (null frame) (display-graphic-p frame))
             (executable-find "systemctl"))
    ;; Do not import into a buffer-local environment managed by envrc/TRAMP.
    (with-temp-buffer
      (let* ((default-directory user-emacs-directory)
             (lines (ignore-errors
                      (process-lines "systemctl" "--user" "show-environment")))
             (values (mapcar
                      (lambda (name)
                        (let* ((prefix (concat name "="))
                               (line (seq-find
                                      (lambda (line) (string-prefix-p prefix line))
                                      lines))
                               (value (and line (substring line (length prefix)))))
                          (cons name (unless (or (null value) (string-empty-p value))
                                       value))))
                      my-desktop-environment--variables))
             (display (cdr (assoc "DISPLAY" values)))
             (authority (cdr (assoc "XAUTHORITY" values)))
             (wayland (cdr (assoc "WAYLAND_DISPLAY" values)))
             (runtime (or (cdr (assoc "XDG_RUNTIME_DIR" values))
                          (getenv "XDG_RUNTIME_DIR")))
             (bus (cdr (assoc "DBUS_SESSION_BUS_ADDRESS" values)))
             ;; `getenv' can synthesize DISPLAY from the selected frame.
             ;; Check the process environment itself to detect SSH forwarding.
             (display-entry (seq-find
                             (lambda (entry) (string-prefix-p "DISPLAY=" entry))
                             process-environment))
             (inherited-display (and display-entry (substring display-entry 8)))
             (inherited-bus (getenv "DBUS_SESSION_BUS_ADDRESS"))
             (frame-display (and frame (frame-parameter frame 'display)))
             (x-ready (and display authority (file-readable-p authority)))
             (wayland-ready (and wayland runtime
                                 (file-exists-p (expand-file-name wayland runtime)))))
        (when (and (or x-ready wayland-ready)
                   (or (not inherited-display) (string-empty-p inherited-display)
                       (equal inherited-display display))
                   (or (not bus) (not inherited-bus) (string-empty-p inherited-bus)
                       (equal inherited-bus bus))
                   (or (not (stringp frame-display))
                       (member frame-display (list display wayland))))
          (dolist (entry values)
            (when (and (cdr entry)
                       (or (not (member (car entry) '("DISPLAY" "XAUTHORITY")))
                           x-ready)
                       ;; A live D-Bus connection keeps its original address.
                       (or (not (equal (car entry) "DBUS_SESSION_BUS_ADDRESS"))
                           (not inherited-bus) (string-empty-p inherited-bus)))
              (setenv (car entry) (cdr entry))))
          (setq my-desktop-environment--imported-p t)
          (remove-hook 'emacs-startup-hook #'my-desktop-environment-import-once)
          (remove-hook 'after-make-frame-functions #'my-desktop-environment-import-once)
          t)))))

(defun my-desktop-environment-setup ()
  "Arrange one desktop import during startup or the first local GUI frame."
  (unless (or noninteractive my-desktop-environment--imported-p)
    (add-hook 'emacs-startup-hook #'my-desktop-environment-import-once)
    (add-hook 'after-make-frame-functions #'my-desktop-environment-import-once)
    (my-desktop-environment-import-once)))

(defvar-local my-auctex-evince--app nil)
(defvar my-auctex-evince--registrations nil)

(defun my-auctex-evince-sync-view ()
  "Forward-search with the Evince D-Bus interface for this buffer."
  (interactive)
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
  (require 'tex)
  (require 'dbus nil t)
  (when (and (not (file-remote-p default-directory))
             (executable-find "evince")
             (or (called-interactively-p 'interactive)
                 (equal (cadr (assq 'output-pdf TeX-view-program-selection))
                        "Evince")))
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
