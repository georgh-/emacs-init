;;; -*- lexical-binding: t -*-
(defun system-windows-p () (string-equal system-type "windows-nt"))
(defun system-mac-p () (string-equal system-type "darwin"))
(defun system-unix-p () (not (or (system-mac-p) (system-windows-p))))


;; Set path to UNIX utilities in Windows
(when (system-windows-p)
  (let ((prepend-directory-to-path
	     (lambda (dir)
	       (setenv "PATH" (concat dir path-separator (getenv "PATH")))
	       (add-to-list 'exec-path dir)))
	    
	    (home (file-name-as-directory
               (concat (getenv "HOMEDRIVE")
                       (getenv "HOMEPATH")))))

    (prepend-directory-to-path
     (file-name-concat home "Software/msys64/usr/bin"))))


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


(defun eval-last-sexp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c C-e") #'eval-last-sexp-and-replace)


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


;; To be able to clear properties (text colors and fonts) in some
;; cases when they are not properly cleared (after copy-paste, or
;; changing between some modes)
(defun clear-all-text-properties ()
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

(use-package doom-modeline
  :config (doom-modeline-mode 1))

;;(package-vc-install '(ultra-scroll :vc-backend Git :url  "https://github.com/jdtsmith/ultra-scroll"))

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(when (system-unix-p)
  (use-package dbus
    :init
    (defun theme-switcher (value)
      ;; 0 = No Preference
      ;; 1 = Prefers dark
      ;; 2 = Prefers light. Not currently used by Gnome
      (let* ((dark-theme 'modus-vivendi)
             (light-theme 'modus-operandi)
             (new-theme (if (= value 1) dark-theme light-theme))
             (switch? (not (member new-theme custom-enabled-themes))))

        (when switch?
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme new-theme))))

    (defun handler (value)
      (theme-switcher (car (car value))))

    (defun signal-handler (namespace key value)
      (if (and
           (string-equal namespace "org.freedesktop.appearance")
           (string-equal key "color-scheme"))
          (theme-switcher (car value))))

    :config
    (dbus-call-method-asynchronously
     :session
     "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.Settings"
     "Read"
     #'handler
     "org.freedesktop.appearance"
     "color-scheme")

    (dbus-register-signal
     :session
     "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.Settings"
     "SettingChanged"
     #'signal-handler)))

(use-package embark
  :bind ("C-S-a" . embark-act))

;; Easily edit files as root
(use-package sudo-edit
  :after embark
  :bind
  (:map embark-file-map ("s" . sudo-edit-find-file))
  (:map embark-become-file+buffer-map ("s" . sudo-edit-find-file)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;;Goes to last changed text in current buffer
(use-package goto-chg
  :bind (("C-," . goto-last-change)
         ("C-." . goto-last-change-reverse)))

(use-package drag-stuff
  ;; Note: does not work with paredit-mode
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

(use-package paredit
  ;; Add paredit to lisp modes
  :hook ((lisp-mode
          emacs-lisp-mode
          lisp-interaction-mode
          ielm-mode)
         . paredit-mode))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

(use-package vertico
  :bind
  (:map vertico-map
        ;; PgUp and PgDn scroll the candidates list
        ("<next>" . vertico-scroll-up)
        ("<prior>" . vertico-scroll-down)))


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
  (remove-function after-focus-change-function #'doom-modeline-focus-change))


;; Enable visual-line mode only for programming modes
;; It will stay disabled for any other mode (occur, packages, etc)
(add-hook 'prog-mode-hook #'visual-wrap-prefix-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-avoid-polling t)
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(byte-compile-verbose nil)
 '(calendar-date-style 'iso)
 '(calendar-week-start-day 1)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(completion-styles '(orderless basic partial-completion emacs22))
 '(context-menu-mode t)
 '(create-lockfiles nil)
 '(debug-on-error nil)
 '(delete-selection-mode t)
 '(desktop-load-locked-desktop 'check-pid)
 '(desktop-restore-frames nil)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-dwim-target t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-listing-switches "-alhv --group-directories-first")
 '(dired-mode-hook '(nerd-icons-dired-mode dired-hide-details-mode))
 '(doc-view-resolution 300)
 '(doom-modeline-mode t)
 '(drag-stuff-global-mode t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-corfu-mode t)
 '(global-page-break-lines-mode t)
 '(grep-highlight-matches t)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(isearch-lazy-count t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(line-number-display-limit-width 1000000)
 '(menu-bar-mode nil)
 '(modus-themes-variable-pitch-ui t)
 '(mouse-yank-at-point t)
 '(nxml-slash-auto-complete-flag t)
 '(org-fontify-emphasized-text nil)
 '(org-fontify-whole-heading-line t)
 '(org-startup-folded nil)
 '(org-superstar-leading-bullet "  ")
 '(org-superstar-special-todo-items t)
 '(org-support-shift-select t)
 '(package-archive-priorities '(("gnu" . 10) ("nongnu" . 9) ("melpa" . 5)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(beginend casual corfu doom-modeline drag-stuff embark goto-chg
              multiple-cursors nerd-icons-dired nerd-icons-ibuffer
              orderless org-superstar page-break-lines paredit
              sudo-edit transient ultra-scroll vertico))
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url
                   "https://github.com/jdtsmith/ultra-scroll")))
 '(savehist-mode t)
 '(show-paren-delay 0.001)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(sudo-edit-indicator-mode t)
 '(sudo-edit-local-method "su")
 '(sudo-edit-remote-method "sudo")
 '(tab-width 4)
 '(temp-buffer-resize-mode t)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'reverse nil (uniquify))
 '(use-package-always-ensure t)
 '(use-short-answers t)
 '(vertico-mode t)
 '(vertico-mouse-mode t)
 '(view-read-only t)
 '(warning-suppress-log-types '((modus-themes) (use-package)))
 '(warning-suppress-types '((comp) (modus-themes) (use-package)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(which-key-mode t)
 '(window-resize-pixelwise t)
 '(winner-mode t))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
