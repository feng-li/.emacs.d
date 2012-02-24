;;; scim-bridge-XX.el  -*- coding: utf-8 -*-

;; Copyright (C) 2008, 2009, 2010 S. Irie
;; Copyright (C) YEAR TRANSLATOR

;; Author: S. Irie
;; Maintainer: TRANSLATOR
;; Keywords: Input Method, i18n

(defconst scim-bridge-XX-version "0.8.2")

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
;; MA  02110-1301 USA

;;; Commentary:

;; The Smart Common Input Method platform (SCIM) is an input
;; method (IM) platform containing support for more than thirty
;; languages (CJK and many European languages) for POSIX-style
;; operating systems including Linux and BSD.

;; scim-bridge.el is a SCIM-Bridge client for Emacs. It allows
;; users on-the-spot style input with SCIM. The input statuses are
;; individually kept for each buffer, and prefix-keys such as C-x
;; and C-c can be used even if SCIM is active. So you can input
;; various languages fast and comfortably by using it.

;; This program changes the documentation strings of the variables
;; and functions defined in scim-bridge.el into the equivalents
;; which are written in English.

;; This program is *not* a part of SCIM-Bridge.

;;
;; Installation:
;;
;; First, save this file (scim-bridge-XX.el) and scim-bridge.el
;; in a directory listed in load-path, and then byte-compile them.
;;
;; Put the following in your .emacs file:
;;
;;   (require 'scim-bridge-XX)
;;   (add-hook 'after-init-hook 'scim-mode-on)
;;
;; To disable XIM in Emacs, put the following in ~/.Xresources:
;;
;;   Emacs*useXIM: false
;;
;; and restart X session or execute a shell command:
;;
;;   xrdb ~/.Xresources
;;
;;
;; Here is the example of settings in .emacs:
;;
;;   (require 'scim-bridge-XX)
;;   ;; Turn on scim-mode automatically after loading .emacs
;;   (add-hook 'after-init-hook 'scim-mode-on)
;;   ;; Use C-SPC for Set Mark command
;;   (scim-define-common-key ?\C-\s nil)
;;   ;; Use C-/ for Undo command
;;   (scim-define-common-key ?\C-/ nil)
;;   ;; Change cursor color depending on SCIM status
;;   (setq scim-cursor-color '("red" "blue" "limegreen"))
;;
;;
;; Note that this program requires GNU Emacs 22 or later, and
;; doesn't work when Emacs is running on terminal emulator.
;;

;; History:
;; YEAR-MM-DD  TRANSLATOR
;;         * Initial release
;;         * Version 0.8.2

;; ToDo:

;;; Code:

(require 'scim-bridge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Apply translations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scim-set-group-doc
 'scim
 ;; "The Smart Common Input Method platform"
 "")

(scim-set-variable-doc
 'scim-mode
 ;; "Toggle scim-mode.
 ;;Setting this variable directly does not take effect;
 ;;use either \\[customize] or the function `scim-mode'."
 "")

;; Basic settings

(scim-set-group-doc
 'scim-basic
 ;; "Settings of operation, such as mode management and keyboard"
 "")

(scim-set-variable-doc
 'scim-mode-local
 ;; "If the value is non-nil, IMContexts are registered for each buffer
 ;;so that the input method of buffers can be toggled individually.
 ;;Otherwise, the input method is globally toggled."
 "")

(scim-set-variable-doc
 'scim-imcontext-temporary-for-minibuffer
 ;; "If non-nil, an one-time IMContext is used for a minibuffer so that
 ;;the minibuffer always starts with SCIM's input status off. This option
 ;;is effective only when the option `scim-mode-local' is active (non-nil)."
 "")

(scim-set-variable-doc
 'scim-use-in-isearch-window
 ;; "If non-nil, SCIM can be used with isearch-mode. Otherwise, it can't.
 ;;
 ;;Note that this option requires SCIM-Bridge version 0.4.13 or later."
 "")

(scim-set-variable-doc
 'scim-use-minimum-keymap
 ;; "If non-nil, printable character events are not handled when SCIM is
 ;;off. This option is useful for avoiding conflict with other Emacs-Lisp
 ;;libraries.
 ;;
 ;;NOTICE: Don't activate this option if SCIM-Bridge version in your system
 ;;is older than 0.4.13, otherwise SCIM cannot receive your inputs."
 "")

(scim-set-variable-doc
 'scim-common-function-key-list
 ;; "This list indicates which keystrokes SCIM takes over at both direct
 ;;insert mode and preediting mode. You can also add/remove the elements
 ;;using the function `scim-define-common-key'.
 ;;NOTICE: Don't set prefix keys in this option, such as ESC and C-x.
 ;;If you do so, operating Emacs might become impossible."
 "")

(scim-set-variable-doc
 'scim-preedit-function-key-list
 ;; "This list indicates which keystrokes SCIM takes over when the
 ;;preediting area exists. You can also add/remove the elements using
 ;;the function `scim-define-preedit-key'."
 "")

(scim-set-variable-doc
 'scim-use-kana-ro-key
 ;; "If you use Japanese kana typing method with jp-106 keyboard, turn
 ;;on (non-nil) this option to input a kana character `ろ' without pushing
 ;;the shift key.
 ;; This option is made effectual by temporarily modifying the X-window
 ;;system's keyboard configurations with a shell command `xmodmap'."
 "")

(scim-set-variable-doc
 'scim-simultaneous-pressing-time
 ;; "If you use Japanese thumb shift typing method on SCIM-Anthy,
 ;;specify the time interval (in seconds) which is corresponding to
 ;;`simultaneous pressing time' setting of SCIM-Anthy. Two keystrokes
 ;;within this time interval are sent to SCIM as a simultaneous keystroke."
 ""
 ;; '(choice (const :tag "none" nil)
 ;;	  (number :tag "interval (sec.)" :value 0.1))
 )

(scim-set-variable-doc
 'scim-undo-by-committed-string
 ;; "If the value is nil, undo is performed bringing some short
 ;;committed strings together or dividing the long committed string
 ;;within the range which does not exceed 20 columns. Otherwise, undo
 ;;is performed to each committed string."
 "")

(scim-set-variable-doc
 'scim-clear-preedit-when-unexpected-event
 ;; "If the value is non-nil, the preediting area is cleared in the
 ;;situations that the unexpected event happens during preediting.
 ;;The unexpected event is, for example, that the string is pasted
 ;;with the mouse."
 "")

;; Appearance

(scim-set-group-doc
 'scim-appearance
 ;; "Faces, candidate window, etc."
 "")

(scim-set-face-doc
 'scim-preedit-default-face
 ;; "This face indicates the whole of the preediting area."
 "")

(scim-set-face-doc
 'scim-preedit-underline-face
 ;; "This face corresponds to the text attribute `Underline' in SCIM
 ;;GUI Setup Utility."
 "")

(scim-set-face-doc
 'scim-preedit-highlight-face
 ;; "This face corresponds to the text attribute `Highlight' in SCIM
 ;;GUI Setup Utility."
 "")

(scim-set-face-doc
 'scim-preedit-reverse-face
 ;; "This face corresponds to the text attribute `Reverse' in SCIM
 ;;GUI Setup Utility."
 "")

(scim-set-variable-doc
 'scim-cursor-color
 ;; "If the value is a string, it specifies the cursor color applied
 ;;when SCIM is on. If a cons cell, its car and cdr are the cursor colors
 ;;which indicate that SCIM is on and off, respectively. If a list, the
 ;;first, second and third (if any) elements correspond to that SCIM is
 ;;on, off and disabled, respectively. The value nil means that the cursor
 ;;color is not controlled at all.
 ;;
 ;;Note that this option requires SCIM-Bridge version 0.4.13 or later."
 ""
 ;;;; ** Don't translate `:value' properties!! **
 ;; '(choice (const :tag "none (nil)" nil)
 ;;	  (color :tag "red" :format "red (%{sample%})\n" :value "red")
 ;;	  (color :tag "blue" :format "blue (%{sample%})\n" :value "blue")
 ;;	  (color :tag "green" :format "green (%{sample%})\n" :value "limegreen")
 ;;	  (color :tag "other" :value "red")
 ;;	  (cons  :tag "other (ON . OFF)"
 ;;		 (color :format "ON: %v (%{sample%})  " :value "red")
 ;;		 (color :format "OFF: %v (%{sample%})\n" :value "blue"))
 ;;	  (list  :tag "other (ON OFF)"
 ;;		 (color :format "ON: %v (%{sample%})  " :value "red")
 ;;		 (color :format "OFF: %v (%{sample%})  DISABLED: none\n"
 ;;			:value "blue"))
 ;;	  (list  :tag "other (ON OFF DISABLED)"
 ;;		 (color :format "ON: %v (%{sample%})  " :value "red")
 ;;		 (color :format "OFF: %v (%{sample%})\n" :value "blue")
 ;;		 (color :format "DISABLED: %v (%{sample%})\n" :value "limegreen")))
 )

(scim-set-variable-doc
 'scim-isearch-cursor-type
 ;; "This option specifies the cursor shape which is applied when
 ;;isearch-mode is active. If an integer 0, this option is not active so
 ;;that the cursor shape is not changed.
 ;;See `cursor-type'."
 ""
 ;; '(choice (const :tag "don't specify (0)" 0)
 ;;	  (const :tag "use frame parameter" t)
 ;;	  (const :tag "don't display" nil)
 ;;	  (const :tag "filled box" box)
 ;;	  (const :tag "hollow box" hollow)
 ;;	  (const :tag "vertical bar" bar)
 ;;	  (cons :tag "vertical bar (specify width)"
 ;;		(const :format "" bar)
 ;;		(integer :tag "width" :value 1))
 ;;	  (const :tag "horizontal bar" hbar)
 ;;	  (cons :tag "horizontal bar (specify height)"
 ;;		(const :format "" hbar)
 ;;		(integer :tag "height" :value 1)))
 )

(scim-set-variable-doc
 'scim-cursor-type-for-candidate
 ;; "This option specifies the cursor shape which is applied when the
 ;;preediting area shows conversion candidates. If an integer 0, this
 ;;option is not active so that the cursor shape is not changed.
 ;;See `cursor-type'."
 ""
 ;; '(choice (const :tag "don't specify (0)" 0)
 ;;	  (const :tag "use frame parameter" t)
 ;;	  (const :tag "don't display" nil)
 ;;	  (const :tag "filled box" box)
 ;;	  (const :tag "hollow box" hollow)
 ;;	  (const :tag "vertical bar" bar)
 ;;	  (cons :tag "vertical bar (specify width)"
 ;;		(const :format "" bar)
 ;;		(integer :tag "width" :value 1))
 ;;	  (const :tag "horizontal bar" hbar)
 ;;	  (cons :tag "horizontal bar (specify height)"
 ;;		(const :format "" hbar)
 ;;		(integer :tag "height" :value 1)))
 )

(scim-set-variable-doc
 'scim-put-cursor-on-candidate
 ;; "When the preediting area shows conversion candidates, the cursor
 ;;is put on the selected segment if this option is non-nil. Otherwise,
 ;;the cursor is put to the tail of the preediting area."
 "")

(scim-set-variable-doc
 'scim-adjust-window-x-position
 ;; "This option specifies whether the position of candidate window
 ;;is adjusted so that the inline candidate and the candidates in that
 ;;window may just line up in the vertical direction. If the value is
 ;;`gnome', the adjustment will be done using the font size setting of
 ;;GNOME desktop environment. Otherwise, if the value is given as an
 ;;integer, that indicates the amount of the gap from normal position
 ;;by the number of pixels.
 ;; This is not suitable for input method of the type to which the
 ;;candidate window is always displayed such as SCIM-pinyin (chinese),
 ;;because there is a possibility that the window hides the cursor when
 ;;the cursor is on the bottom of screen."
 ""
 ;; '(choice (const :tag "use GNOME's font size" gnome)
 ;;	  (integer :tag "specify by pixel number" :value 24)
 ;;	  (const :tag "off" nil))
 )

(scim-set-variable-doc
 'scim-prediction-window-position
 ;; "(For Japanese IM only) The value should be given as (POS . ADJ).
 ;;If POS is non-nil, the forecast window is displayed under the head
 ;;of the preediting area. If the value of ADJ is non-nil, the horizontal
 ;;position of it is adjusted same as `scim-adjust-window-x-position'."
 ""
 ;; '(cons
 ;;   (choice :tag "Position"
 ;;	   (const :tag "Tail of preediting area" nil)
 ;;	   (const :tag "Head of preediting area" t))
 ;;   (choice :tag "Adjustment"
 ;;	   (const :tag "same as conversion window" t)
 ;;	   (const :tag "off" nil)))
 )

(scim-set-variable-doc
 'scim-mode-line-string
 ;; "This variable specify a string that appears in the mode line
 ;;when scim-mode is active, and not otherwise. This string should be
 ;;a short string which starts with a space and represents scim-mode."
 "")

;; Advanced settings

(scim-set-group-doc
 'scim-expert
 ;; "Advanced settings"
 "")

(scim-set-variable-doc
 'scim-focus-update-interval
 ;; "The window focus is checked with this cycle measured in seconds.
 ;;When SCIM is off or input focus is in the other application, the slower
 ;;time cycle given by `scim-focus-update-interval-long' is used instead.
 ;;
 ;;Note that this value is not used if SCIM-Bridge version in your system
 ;;is older than 0.4.13 or your window manager does not support a property
 ;;`_NET_ACTIVE_WINDOW'. In that case, `scim-focus-update-interval-long'
 ;;is used at all times."
 "")

(scim-set-variable-doc
 'scim-focus-update-interval-long
 ;; "This value might be used as a slow time cycle for the observation
 ;;of input focus instead of `scim-focus-update-interval'.
 ;;
 ;;See `scim-focus-update-interval' for details."
 "")

(scim-set-variable-doc
 'scim-kana-ro-x-keysym
 ;; "When Japanese kana-RO key is used, this option specifies the
 ;;substitute KeySym name used in X window system for the key. This
 ;;program sets the substitute KeySym for backslash key to distinguish
 ;;it from yen-mark key."
 "")

(scim-set-variable-doc
 'scim-kana-ro-key-symbol
 ;; "When Japanese kana-RO key is used, this option specifies the event
 ;;corresponding to the substitute KeySym given in `scim-kana-ro-x-keysym'
 ;;as a symbol. This program sets the substitute KeySym for backslash key
 ;;to distinguish it from yen-mark key."
 ""
 ;; '(choice (symbol)
 ;;	  (const :tag "none" nil))
 )

(scim-set-variable-doc
 'scim-bridge-timeout
 ;; "Specify the maximum waiting time for data reception from SCIM.
 ;;A floating point number means the number of seconds, otherwise an integer
 ;;the milliseconds."
 "")

(scim-set-variable-doc
 'scim-bridge-x-display-substitute
 ;; "Don't set this variable unless you want to explicitly specify the
 ;;X display number and screen number. Setting this variable makes
 ;;scim-mode unusable in multi-display environment.
 ;;
 ;;If you set this variable, the value must be a string such as \":0.0\"."
 "")

(scim-set-variable-doc
 'scim-config-file
 ;; "The name of SCIM's configuration file, which is used to detect
 ;;the change of SCIM settings."
 "")

(scim-set-variable-doc
 'scim-meta-key-exists
 ;; "t is set in this variable if there is mata modifier key in the
 ;;keyboard. When automatic detection doesn't go well, please set the
 ;;value manually before scim-bridge.el is loaded."
 "")

(scim-set-variable-doc
 'scim-tmp-buffer-name
 ;; "This is working buffer name used for communicating with the agent."
 "")

(scim-set-variable-doc
 'scim-incompatible-major-modes
 ;; "List of symbols specifying major modes that keymaps of scim-mode are
 ;;deactivated."
 "")

(scim-set-variable-doc
 'scim-incompatible-mode-hooks
 ;; "List of symbols specifying major mode hooks that keymaps of scim-mode
 ;;are deactivated when invoking these hooks."
 "")

(scim-set-variable-doc
 'scim-preedit-incompatible-commands
 ;; "List of symbols specifying commands which are disabled when preediting."
 "")

(scim-set-variable-doc
 'scim-inherit-im-functions
 ;; "List of symbols specifying functions which inherit input method.
 ;;If the function takes the argument INHERIT-INPUT-METHOD, input method
 ;;is inherited only when it's non-nil. Otherwise, input method is
 ;;unconditionally inherited."
 "")

;; Functions

(scim-set-function-doc
 'scim-define-common-key
 ;; "Specify which key events SCIM anytime takes over. If HANDLE
 ;;is non-nil, SCIM handles the key events given by KEY. When KEY is
 ;;given as an array, it doesn't indicate key sequence, but multiple
 ;;definitions of single keystroke.
 ;; It is necessary to call a function `scim-update-key-bindings' or
 ;;restart scim-mode so that this settings may become effective."
 "")

(scim-set-function-doc
 'scim-define-preedit-key
 ;; "Specify which key events SCIM takes over when preediting. If
 ;;HANDLE is non-nil, SCIM handles the key events given by KEY. When
 ;;KEY is given as an array, it doesn't indicate key sequence, but
 ;;multiple definitions of single keystroke.
 ;; It is necessary to call a function `scim-update-key-bindings' or
 ;;restart scim-mode so that this settings may become effective."
 "")

(scim-set-function-doc
 'scim-enable-isearch
 ;; "Make SCIM usable with isearch-mode."
 "")

(scim-set-function-doc
 'scim-disable-isearch
 ;; "Make SCIM not usable with isearch-mode."
 "")

(scim-set-function-doc
 'scim-mode-on
 ;; "Turn scim-mode on."
 "")

(scim-set-function-doc
 'scim-mode-off
 ;; "Turn scim-mode off."
 "")

(scim-set-function-doc
 'scim-mode
 ;; "Toggle SCIM minor mode (scim-mode).
 ;;With optional argument ARG, turn scim-mode on if ARG is
 ;;positive, otherwise turn it off."
 "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Define functions useful only for TARGET_LANGUAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings

;; Functions

;;  (THIS SECTION IS OPTIONAL)

(provide 'scim-bridge-XX)

;;;
;;; scim-bridge-XX.el ends here
