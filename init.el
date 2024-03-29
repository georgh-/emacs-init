;; This file is public domain

;; Manually installed libraries
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

;; LilyPond Emacs modes are installed here
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(defalias 'yes-or-no-p #'y-or-n-p)

;; Add a hook function to multiple mode hooks
(defun add-to-multiple-hooks (func hooks)
  (mapc (lambda (hook) (add-hook hook func))
        hooks))

;; Show file name in title bar and modification status
;;
;; https://www.emacswiki.org/emacs/FrameTitle
(setq frame-title-format
      '((:eval (if (and (buffer-modified-p)
                        (buffer-file-name))
                   "* "))
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " - Emacs"))

(defun system-windows-p () (string-equal system-type "windows-nt"))
(defun system-mac-p () (string-equal system-type "darwin"))
(defun system-unix-p () (not (or (system-mac-p) (system-windows-p))))

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it.
Uses `copy-region-as-kill'."
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (message "Buffer content saved to kill ring."))

(global-set-key (kbd "C-c w") #'copy-buffer-as-kill)

(defun remove-system-clipboard-format ()
  "Remove format from system clipboard.
Pastes the contents of the system clipboard to Emacs and copies
it again to the clipboard to ensure that system clipboard
contains unformatted text.  Useful when copying/pasting between
Browsers/Word/Email/etc.

Emacs' kill ring is unmodified after running this function."
  (interactive)
  (with-temp-buffer
    (clipboard-yank)
    (pop kill-ring)

    (clipboard-kill-region (point-min) (point-max))
    (pop kill-ring)

    (message "Format has been removed from system clipboard text.")))

(global-set-key (kbd "C-c r") #'remove-system-clipboard-format)

;; Use M-o to switch between windows instead of C-x o. M-o is normally a prefix
;; command that I never use.
(global-set-key (kbd "M-o") #'other-window)

(defun kill-save-line (nlines)
  "Kill line without deleting it. Includes newline character."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-end-position nlines))
  (kill-append "\n" nil)
  (message "Saved line to kill-ring"))

(global-set-key (kbd "M-k") #'kill-save-line)

;;;
;;; Packages' configuration.
;;;

;; Options must be set before macro is expanded, which happens when
;; the function body is compiled. Therefore, options set inside the
;; function, or below the function will not take effect, because the
;; macro has already been expanded before the options have been set
(require 'use-package-ensure)
(setq use-package-always-ensure t)


(defun my-after-init-function ()
  ;; Configuration that must be run after init to ensure that the preferences
  ;; set up by Customise are taken into account

  (use-package ef-themes)

  ;; General
  (use-package dbus
    :init
    (defun theme-switcher (value)
      ;; 0 = No Preference
      ;; 1 = Prefers dark
      ;; 2 = Prefers light. Not currently used by Gnome
      (let* ((dark-theme 'ef-dark)
             (light-theme 'ef-frost)
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
     #'signal-handler))

  (use-package doom-modeline
    :init
    (doom-modeline-mode)

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

  ;; Selection framework using native Emacs API

  (use-package vertico
    :init
    (vertico-mode 1)
    (vertico-mouse-mode 1))

  ;; Filtering function

  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles
                                            basic
                                            partial-completion)))))

  ;; Add actions to minibuffer selections and other commands

  (use-package embark
    :bind
    ("C-S-a" . embark-act)) ;; alternative for `describe-bindings'

  (use-package embark-consult
    :ensure t ; only need to install it, embark loads it after consult if found
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))

  ;; Add descriptions to minibuffer selections
  (use-package marginalia
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  ;; Consult extended commands
  (use-package consult)

  ;; UI for completion on buffer completion-in-region-function (aka
  ;; intellisense)
  (use-package corfu
    :init
    (global-corfu-mode))

  ;; Complete at point extensions
  (use-package cape
    :init
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    ;;(add-to-list 'completion-at-point-functions #'cape-history)
    ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
    ;;(add-to-list 'completion-at-point-functions #'cape-tex)
    ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
    ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
    ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
    ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
    ;;(add-to-list 'completion-at-point-functions #'cape-dict)
    ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
    ;;(add-to-list 'completion-at-point-functions #'cape-line)
    )

  ;; Easily edit files as root
  (use-package sudo-edit
    :after embark
    :bind
    (:map embark-file-map
          ("s" . sudo-edit-find-file))
    (:map embark-become-file+buffer-map
          ("s" . sudo-edit-find-file)))

  ;; Shows help for some commands
  (use-package discover)

  ;; Smarter placement of cursor at begining of buffer M-< M->
  (use-package beginend
    :config

    ;; Add beginend for all supported modes
    (beginend-setup-all))

  (use-package magit :defer t)

  (use-package vterm :defer t)

  ;; Show a horizontal line instead of ^L character (new page character)
  ;; May have bad interactions with adaptive-wrap
  (use-package page-break-lines
    :config (global-page-break-lines-mode))

  (use-package hide-lines :defer t)

  ;; Shows key shortcuts and commands while typing a keyboard shortcut
  ;; For example, type C-c and wait, and it will show a guide
  (use-package which-key
    :config (which-key-mode 1))

  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  (use-package multiple-cursors
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C->" . mc/mark-all-like-this)))

  ;; Goes to last changed text in current buffer
  (use-package goto-chg
    :bind (("C-," . goto-last-change)
           ("C-." . goto-last-change-reverse)))

  (use-package macrostep :defer t)

  (use-package all-the-icons :defer t)
  (use-package all-the-icons-dired
    :after all-the-icons
    :hook (dired-mode . all-the-icons-dired-mode))

  ;; (use-package neotree
  ;;   :bind (("<f9>" . neotree-show)
  ;;          (:map neotree-mode-map ("<f9>" . neotree-hide))))

  ;; Keyboard

  (use-package key-chord
    :init
    (key-chord-define-global "xk" #'kill-current-buffer)
    (key-chord-define-global "x0" #'delete-window)
    (key-chord-define-global "x1" #'delete-other-windows)
    (key-chord-define-global "x2" #'split-window-below)
    (key-chord-define-global "x3" #'split-window-right)
    (key-chord-define-global "xb" #'switch-to-buffer)
    (key-chord-define-global "p[" #'previous-buffer)
    (key-chord-define-global ";'" #'next-buffer)
    (key-chord-define-global "xf" #'find-file)
    (key-chord-define-global "xs" #'save-buffer)
    (key-chord-define-global "xw" #'write-file))

  (use-package use-package-chords
    :init (key-chord-mode 1))

  (use-package drag-stuff
    ;; Note: does not work with paredit-mode
    :bind (("M-<up>" . drag-stuff-up)
           ("M-<down>" . drag-stuff-down))
    :init (drag-stuff-global-mode))

  (use-package paredit
    ;; Add paredit to lisp modes
    :hook ((lisp-mode
            emacs-lisp-mode
            lisp-interaction-mode
            ielm-mode)
           . paredit-mode))

  (use-package puni
    ;; like paredit but for non-lisp modes
    :defer t
    :hook ((prog-mode
            sgml-mode
            nxml-mode
            tex-mode)
           . puni-mode)
    :hook ((lisp-mode
            emacs-lisp-mode
            lisp-interaction-mode
            ielm-mode)
           . puni-disable-puni-mode))

  ;; Syntax analyzer (coding modes), spellchecker (non-coding modes)
  (use-package flymake
    :hook prog-mode)

  (use-package emacs
    :hook ((tex-mode
            text-mode)
           . flyspell-mode))

  ;; Language modes

  (use-package treesit-auto
    :config
    (global-treesit-auto-mode))

  (use-package yaml-mode :defer t)

  (use-package eglot
    :defer t
    :hook (haskell-mode . eglot-ensure))

  (use-package haskell-mode
    :defer t
    :bind (:map
           haskell-mode-map
           ("C-c C-l" . haskell-process-load-file)
           ("C-`"     . haskell-interactive-bring)
           ("C-c C-t" . haskell-process-do-type)
           ("C-c C-i" . haskell-process-do-info)
           ("C-c C-c" . haskell-process-cabal-build)
           ("C-c C-k" . haskell-interactive-mode-clear)

           :map
           haskell-cabal-mode-map
           ("C-c c"   . haskell-process-cabal)
           ("C-`"     . haskell-interactive-bring)
           ("C-c C-k" . haskell-interactive-mode-clear)
           ("C-c C-c" . haskell-process-cabal-build)
           ("C-c c"   . haskell-process-cabal)))

  ;; (use-package hlint-refactor
  ;;   :after flycheck
  ;;   :hook (haskell-mode . hlint-refactor-mode))

  ;; (use-package haskell-snippets)

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode))

  (use-package yaml-mode :defer t)
  (use-package nhexl-mode :defer t)
  (use-package markdown-mode :defer t)
  (use-package toml-mode :defer t)

  (use-package web-mode
    :mode ("\\.html?\\'"
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.[agj]sp\\'"
           "\\.as[cp]x\\'"
           "\\.erb\\'"
           "\\.mustache\\'"
           "\\.djhtml\\'"))

  (use-package powershell :defer t))
(add-hook 'after-init-hook #'my-after-init-function)

;; Use simple dired by default
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Enable visual-line mode only for programming modes
;; It will stay disabled for any other mode (occur, packages, etc)
(defun enable-visual-line-mode ()
  (visual-line-mode t))
(add-hook 'prog-mode-hook #'enable-visual-line-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

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

;; When killing a buffer, I always choose the current one. If I want to kill
;; other buffers, there are other mechanisms:
;;  * Switch to the buffer (C-x b) and kill it
;;  * M-x kill-some-buffers
;;  * C-x C-b (ido buffer in my configuration)
(defun kill-current-buffer ()
  "Kills current buffer, does not ask which buffer to kill."
  (interactive)
  (kill-buffer (buffer-name)))
(global-set-key (kbd "C-x k") #'kill-current-buffer)

(add-hook 'org-mode-hook #'enable-visual-line-mode)


;; Use ibuffer instead of default
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Configure default face
;; (let ((font "Dejavu Sans Mono-10"))
;;   (add-to-list 'initial-frame-alist `(font . ,font))
;;   (add-to-list 'default-frame-alist `(font . ,font)))

;; Use variable spaced font in configuration and info
(defun enable-variable-pitch-mode ()
  (variable-pitch-mode 1))

;; Enable proportional fonts in the modes listed below
(let ((variable-pitch-modes
       '(info-mode-hook
         custom-mode-hook
         help-mode-hook
         neotree-mode-hook)))

  (add-to-multiple-hooks #'enable-variable-pitch-mode
                         variable-pitch-modes))

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

;; To be able to clear properties (text colors and fonts) in some cases when
;; they are not properly cleared (after copy-paste, or changing between
;; some modes)
(defun clear-all-text-properties ()
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil)))

;; From http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml-region (begin end separate-attrs)
  "Pretty format XML markup in region. Only works in XML/HTML
modes. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "rP")
  (save-excursion
    (goto-char begin)
    (if separate-attrs
        (while (search-forward-regexp "\"[ \\t]+" nil t)
          (backward-char) (insert "\n") (setq end (1+ end))))
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Indented XML."))

(defun pretty-print-xml-buffer (separate-attrs)
  (interactive "P")
  (pretty-print-xml-region (point-min) (point-max) separate-attrs))

(defun delete-trailing-whitespace-on-save ()
  (when (derived-mode-p '(prog-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook #'delete-trailing-whitespace-on-save)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine 'luatex)
 '(auth-source-save-behavior nil)
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(calendar-date-style 'iso)
 '(calendar-week-start-day 1)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(context-menu-mode t)
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   '("2141b59c9b098b476a7e20f7a621985b5d89544ae22a8d4b79b574f1203b6496" "471b78fcfb7a535680b1a9773870d1525389fd2c5559d5707940d0ecc181eb69" "f126b518f12b4f6bd50808143f7bd26c1d47de25d90170d3d632a46c2a08a1af" "5ccda8419d11dec4afc7d5afc71de75e76f2d0ce6e845bf6831582c67ee79086" "d47e82e61cffed27dd2aef3b614f6dd727776f6bcb92e738e89056b325a5aeab" "032426ec19e515fd3a54b38016a1c5e4ec066be3230198cb3df82d05630a02ed" "aee4c6b492ad130f13868464e4d7f2b2846de9b7f0d2933499c907f47dc010f4" "02790c735d32ad3b28c630329fdfc503ea62077d088b0c52302ab61e5a3b037e" "41bbaed6a17405ee6929c7e1f8035cffd05d0ebf3f08ce388da0e92c63fb6cef" "f5f3921b9cec1b37758ba865127d773f8f5e4816e63712af7582b447acfa5326" "910b36cacb8486580842582661ab2f16d8e05e6ec081dcaa141e0ca98ee5e9c2" "c06aa0ddb649e4e45f36dd95de98263672864074373937e65a23c8338f52c6af" "8294b451ffe0575fcccd1a447f56efc94d9560787cd5ff105e620e5f5771427d" "c6b317b294f9e0ecf7290a6d76b4c96ffd52213cdcb3fdad5db29141c63866cf" "d13b6ae136b853bc69c036009b6290f546e6c9c7ad026f60b0ce2a4f9a943d5f" "e0aaf54e0194bd9f452ae36f0012b23d3f82d2092e2b800cc07e0e73f4ac131f" "bcfeecf5f2ee0bbc64450f7c5155145d8d2c590b1310a898c505f48b4b5f4c75" "e5a748cbefd483b74b183d7da4fca6228207a6bf9be9792dc85403a186724e1f" "20d3ce5f5cb95716edca608ef7bbc27d9f8d66c9a51200f7be3f08c107810f3e" "13f343f7d098365848ba4366801a9ae91c35faea85b017818fd4d07dfd18de61" "68b35e92f9daa37685218bd11aa5307140a0ec4c8fd17142a83457619e7b1240" "49887e6f0c666dfc10fad4c23c7a83a176cb296968648c02b85deec25bb11103" default))
 '(debug-on-error nil)
 '(delete-selection-mode t)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-dnd-protocol-alist nil)
 '(dired-dwim-target t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-listing-switches "-alh --group-directories-first")
 '(doc-view-resolution 300)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ef-themes-mixed-fonts t)
 '(ef-themes-to-toggle '(ef-frost ef-dark))
 '(ef-themes-variable-pitch-ui t)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-discover-mode t)
 '(global-visual-line-mode nil)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(isearch-lazy-count t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(line-number-display-limit-width 1000000)
 '(menu-bar-mode nil)
 '(modus-themes-mixed-fonts t)
 '(modus-themes-variable-pitch-ui t)
 '(mouse-yank-at-point t)
 '(nxml-slash-auto-complete-flag t)
 '(org-fontify-emphasized-text nil)
 '(org-fontify-whole-heading-line t)
 '(org-startup-folded nil)
 '(org-superstar-leading-bullet "  ")
 '(org-superstar-special-todo-items t)
 '(org-support-shift-select t)
 '(org-use-speed-commands t)
 '(package-archive-priorities '(("gnu" . 1) ("nongnu" . 1)))
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(puni restclient treesit-auto php-mode embark-consult doom-modeline ef-themes cape writeroom-mode corfu consult docker-mode all-the-icons-dired all-the-icons beginend discover drag-stuff embark expand-region goto-chg haskell-mode hide-lines key-chord macrostep magit marginalia markdown-mode modus-themes multiple-cursors nhexl-mode orderless org-superstar org-superstar-mode page-break-lines paredit powershell sudo-edit toml-mode use-package use-package-chords vertico vterm web-mode which-key yaml-mode))
 '(ring-bell-function 'ignore)
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
 '(view-read-only t)
 '(visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
 '(warning-suppress-log-types '((modus-themes) (use-package)))
 '(warning-suppress-types '((comp) (modus-themes) (use-package)))
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-script-padding 2)
 '(which-key-lighter "")
 '(which-key-mode t)
 '(which-key-side-window-max-height 0.6)
 '(whitespace-style
   '(face tabs spaces newline indentation space-mark tab-mark newline-mark))
 '(window-resize-pixelwise t)
 '(winner-mode t))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :family "Liberation Mono")))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
