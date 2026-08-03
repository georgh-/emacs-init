;;; -*- lexical-binding: t -*-

;; Use M-o to switch between windows instead of C-x o
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "<remap> <kill-buffer>" #'kill-current-buffer)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-other-window)


(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it.
Uses `copy-region-as-kill'."
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (message "Buffer content saved to kill ring."))

(keymap-global-set "C-c w" #'copy-buffer-as-kill)


(defun remove-system-clipboard-format ()
  "Remove format from system clipboard.
Pastes the contents of the system clipboard to Emacs and copies
it again to the clipboard to ensure that system clipboard
contains unformatted text.  Useful when copying/pasting between
Browsers/Word/Email/etc.

Emacs' kill ring is not affected by this function."
  (interactive)
  (let ((clipboard-text (gui-get-selection 'CLIPBOARD)))
    (gui-set-selection 'CLIPBOARD clipboard-text)))

(keymap-global-set "C-c r" #'remove-system-clipboard-format)


(defun kill-save-line (nlines)
  "Kill line without deleting it. Includes newline character."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-end-position nlines))
  (kill-append "\n" nil)
  (message "Saved line to kill-ring"))

(keymap-global-set "M-k" #'kill-save-line)


(defun eval-last-sexp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(keymap-global-set "C-c C-e" #'eval-last-sexp-and-replace)


(defun indent-buffer ()
  "Indent all buffer using indent-region."
  (interactive)
  (indent-region (point-min) (point-max)))


;; http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))


(defun clear-all-text-properties ()
  "Clear properties (text colors and fonts).

In some cases they are not properly cleared such as after copy-paste, or
changing between some modes."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))


;; Show file name in title bar and modification status
;;
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format
      '((:eval (when (and (buffer-modified-p) (buffer-file-name))
                 "* "))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs"))


(load-theme 'newcomers-presets)


(use-package emacs
  :ensure nil
  :custom
  ;; UI
  (blink-cursor-mode nil)
  (blink-matching-paren nil)
  (column-number-indicator-zero-based nil)
  (custom-theme-allow-multiple-selections t)
  (help-window-select t)
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (ring-bell-function 'ignore)
  (tool-bar-mode nil)
  (uniquify-buffer-name-style 'reverse nil (uniquify))
  (use-short-answers t)
  (window-resize-pixelwise t)
  (winner-mode t)

  ;; File management
  (auto-revert-avoid-polling t)
  (global-auto-revert-mode t)
  (create-lockfiles nil)
  
  ;; Editor
  (desktop-load-locked-desktop 'check-pid)
  (desktop-restore-frames nil)
  (global-visual-line-mode nil)
  (isearch-lazy-count t)
  (line-number-display-limit-width 1000000)
  (show-paren-delay 0.001)
  (show-paren-style 'expression)
  (tab-width 4)
  (temp-buffer-resize-mode t)
  (view-read-only t)

  ;; mode-specific
  (calendar-date-style 'iso)
  (calendar-week-start-day 1)
  (doc-view-resolution 300)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (grep-highlight-matches t)
  (js-indent-level 2)
  (js-switch-indent-offset 2)
  (nxml-slash-auto-complete-flag t)
  
  ;; Org-mode
  (org-fontify-emphasized-text nil)
  (org-fontify-whole-heading-line t)
  (org-special-ctrl-a/e t)
  (org-startup-folded nil)
  (org-superstar-leading-bullet "  ")
  (org-superstar-special-todo-items t)
  (org-support-shift-select t)
  (org-use-speed-commands t)
  
  ;; Warnings
  (byte-compile-verbose nil)
  (debug-on-error nil)
  (native-comp-async-report-warnings-errors nil)
  (warning-suppress-log-types '((bytecomp) (modus-themes) (use-package)))
  (warning-suppress-types '((bytecomp) (modus-themes) (use-package)))

  ;; web-mode
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2))

(use-package package
  :ensure nil
  :custom
  (use-package-always-ensure t)
  (package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/"))))

(use-package modus-themes
  :ensure nil
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle '(ef-frost ef-dark))
  (modus-themes-variable-pitch-ui t))

(use-package ef-themes
  :custom
  (modus-themes-common-palette-overrides
   '((border-mode-line-active bg-mode-line-active)
     (border-mode-line-inactive bg-mode-line-inactive))))

;; Theme
(defun dark-theme-p ()
  (cond (system-gnu?
         (require 'dbus)
         (let* ((dbus-value
                 (dbus-call-method
                  :session
                  "org.freedesktop.portal.Desktop"
                  "/org/freedesktop/portal/desktop"
                  "org.freedesktop.portal.Settings"
                  "Read" ;; Dbus method
                  "org.freedesktop.appearance"
                  "color-scheme"))

                (value (car (car dbus-value))))

           ;; 0 = No Preference
           ;; 1 = Prefers dark
           ;; 2 = Prefers light. Not currently used by Gnome
           (= value 1)))

        (t nil)))


(cond
 (system-windows?
  (load-theme 'modus-operandi t nil))

 (system-gnu?
  (defun theme-switcher (dark?)
    (let* ((dark-theme 'ef-dark)
           (light-theme 'ef-frost)
           (new-theme (if dark? dark-theme light-theme))
           (switch? (not (member new-theme custom-enabled-themes))))

      (when switch?
        (disable-theme dark-theme)
        (disable-theme light-theme)
        (load-theme new-theme t nil))))

  (defun dbus-signal-theme-handler (namespace key value)
    (when (and
           (string-equal namespace "org.freedesktop.appearance")
           (string-equal key "color-scheme"))
      ;; 0 = No Preference
      ;; 1 = Prefers dark
      ;; 2 = Prefers light. Not currently used by Gnome
      (theme-switcher (= 1 (car value)))))

  (require 'dbus)
  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   "/org/freedesktop/portal/desktop"
   "org.freedesktop.portal.Settings"
   "SettingChanged"
   #'dbus-signal-theme-handler)

  (theme-switcher (dark-theme-p))))

;; Easily edit files as root
(use-package sudo-edit
  :custom
  (sudo-edit-indicator-mode t)
  (sudo-edit-local-method "su")
  (sudo-edit-remote-method "sudo"))

(use-package expreg
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)
      
         (:repeat-map expreg-repeat-map
                      ("=" . expreg-expand)
                      ("-" . expreg-contract))))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;;Goes to last changed text in current buffer
;; (use-package goto-chg
;;   :bind (("C-," . goto-last-change)
;;          ("C-." . goto-last-change-reverse)))

(use-package drag-stuff
  ;; Note: does not work with paredit-mode
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package paredit
  ;; Add paredit to lisp modes
  :hook (lisp-mode
         emacs-lisp-mode
         lisp-interaction-mode
         ielm-mode))

(use-package minibuffer
  :ensure nil
  :custom
  (completion-auto-help t)
  (completion-eager-display t)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completions-format 'one-column)
  (completions-max-height 14)
  (completions-sort 'historical)
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (minibuffer-prompt-properties
   '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
  (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (orderless-expand-substring nil)
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion
                                       ;; behaves like substring

;; Smarter placement of cursor at begining of buffer M-< M->
(use-package beginend
  :config

  ;; Add beginend for all supported modes
  (beginend-setup-all))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  '(dired-dwim-target t)
  '(dired-hide-details-hide-symlink-targets nil)
  '(dired-listing-switches "-alhv --group-directories-first"))

(use-package nerd-icons-dired
  :hook dired-mode)

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package page-break-lines
  :config (global-page-break-lines-mode))

(use-package doom-modeline
  :config

  ;; Emacs always keeps one window active across all frames, even when no
  ;; frames have focus (before 2002-02-09, inactive mode-lines did not
  ;; exist). Doom-modeline attempts to make all windows look inactive
  ;; when Emacs looses the focus. To achieve that, it sets and unsets the
  ;; "mode-line-inactive" faces for all the properties used in the
  ;; mode-line. As a consequence, the mode-line icons may look as text if
  ;; the inactive mode-line specifies a font family, which happens with
  ;; modus-themes and ef-themes (because the font family does not contain
  ;; icons).
  ;;
  ;; Ensure that Emacs default behavior is respected (one window is
  ;; always active regardless whether Emacs has focus or not)
  (advice-remove #'handle-switch-frame 'doom-modeline-focus-change)
  (remove-function after-focus-change-function
                   #'doom-modeline-focus-change)

  (doom-modeline-mode 1)
  
  :custom
  (doom-modeline-column-zero-based nil)
  (doom-modeline-mode t)
  (doom-modeline-position-column-format '("C%C")))

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0)

  :config
  (ultra-scroll-mode 1))

;; Enable visual-line mode only for programming and org modes
(use-package visual-wrap
  :ensure nil
  :hook ((prog-mode org-mode) . visual-wrap-prefix-mode))

(use-package display-line-numbers
  :ensure nil
  :hook prog-mode)

;; (use-package magit)

;; (use-package markdown-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("725195e919c94667dfbe186161d63f11799b93d74e846ec1404900f34d320c79"
     "a8c1252f9844caf313a2315ecf1e8ef4d92495c9f2067d875bb1c783b08719ad"
     "0dd83cb583518e6a20cd7881e4d2251c80c1141b50dc29fbe13198e62f3620f6"
     default))
)

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
