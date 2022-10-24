;; This file is public domain

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 100 1024 1024))

;; After starting, set it to 1MiB
(defun restore-gc-threshold ()
  (setq gc-cons-threshold (* 1 1024 1024)))

(add-hook 'emacs-startup-hook #'restore-gc-threshold)

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

(defun my-after-init-function ()
  ;; Configuration that must be run after init to ensure that the preferences
  ;; set up by Customise are taken into account

  (package-initialize)
  (unless (package-installed-p 'use-package)
    (message "Initializing and refreshing ELPA package archives...")
    (package-refresh-contents)
    (message "Installing `use-package'...")
    (package-install 'use-package))
  (require 'use-package-ensure)

  ;; General

  (when (and (system-unix-p)
             (display-graphic-p))
    (use-package dbus)
    (use-package modus-themes
      :after (dbus)
      :config
      (defun set-modus-theme-from-gtk ()
        "Set modus theme by checking whether GTK theme is dark."
        (let ((gtk-theme (downcase
                          (call-process-string "gsettings"
                                               "get"
                                               "org.gnome.desktop.interface"
                                               "gtk-theme"))))
          (if (or (string-match-p "dark"  gtk-theme)
                  (string-match-p "black" gtk-theme))
              (modus-themes-load-vivendi)
            (modus-themes-load-operandi))))

      (defun gtk-theme-changed (path _ _)
        "DBus handler to detect when the GTK theme has changed."
        (when (string-equal path "/org/gnome/desktop/interface/gtk-theme")
          (set-modus-theme-from-gtk)))

      (dbus-register-signal
       :session
       "ca.desrt.dconf"
       "/ca/desrt/dconf/Writer/user"
       "ca.desrt.dconf.Writer"
       "Notify"
       #'gtk-theme-changed)

      (set-modus-theme-from-gtk)))

  (when (or (system-windows-p)
            (not (display-graphic-p)))
    (use-package modus-themes
      :config
      (modus-themes-load-operandi)))

  (use-package doom-modeline
    :defer nil
    :config
    (setq doom-modeline-buffer-file-name-style 'relative-to-project)
    (doom-modeline-mode t))

  ;; Selection framework using native Emacs API
  (use-package selectrum
    :init
    (selectrum-mode 1))

  ;; Filtering function
  (use-package orderless
    :custom
    (completion-styles '(orderless)))
    
  ;; Add actions to minibuffer selections and other commands
  (use-package embark
    :bind
    (("C-S-a" . embark-act)        ;; pick some comfortable binding
     ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

  ;; Add descriptions to minibuffer selections
  (use-package marginalia
    :bind (:map minibuffer-local-map
                ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))


  ;; M-n and M-p go to next or previous symbol matching symbol under cursor
  ;; (use-package smartscan
  ;;   :config (global-smartscan-mode 1))

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

  (use-package magit
    :bind ("<f10>" . #'magit-status))

  ;; Show a horizontal line instead of ^L character (new page character)
  ;; May have bad interactions with adaptive-wrap
  (use-package page-break-lines
    :config (global-page-break-lines-mode))

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
  
  (use-package neotree
    :bind (("<f9>" . neotree-show)
           (:map neotree-mode-map ("<f9>" . neotree-hide))))
  
  ;; Keyboard

  (use-package key-chord
    :init
    (key-chord-define-global "xk" #'kill-current-buffer)
    (key-chord-define-global "x0" #'delete-window)
    (key-chord-define-global "x1" #'delete-other-windows)
    (key-chord-define-global "x2" #'split-window-below)
    (key-chord-define-global "x3" #'split-window-right)
    (key-chord-define-global "xb" #'switch-to-buffer)
    (key-chord-define-global "kp" #'previous-buffer)
    (key-chord-define-global "km" #'next-buffer)
    (key-chord-define-global "xf" #'find-file)
    (key-chord-define-global "xs" #'save-buffer)
    (key-chord-define-global "xw" #'write-file))

  (use-package use-package-chords
    :init (key-chord-mode 1))

  (use-package drag-stuff
    :bind (("M-<up>" . drag-stuff-up)
           ("M-<down>" . drag-stuff-down))
    :init (drag-stuff-global-mode))

  ;; Complete Anything - Code completion framework
  (use-package company
    :config (global-company-mode 1))

  ;; Syntax analyzer (coding modes), spellchecker (non-coding modes)
  (use-package flycheck
    :hook ((text-mode . flycheck-mode)
           (prog-mode . flycheck-mode)))

  ;; Language modes

  (use-package tree-sitter
    :config (global-tree-sitter-mode t))
  (use-package tree-sitter-langs)
  (use-package tree-sitter-indent)

  (use-package yaml-mode :defer t)

  (use-package haskell-mode
    :bind (:map haskell-mode-map
                ("C-c C-l" . haskell-process-load-file)
                ("C-`"     . haskell-interactive-bring)
                ("C-c C-t" . haskell-process-do-type)
                ("C-c C-i" . haskell-process-do-info)
                ("C-c C-c" . haskell-process-cabal-build)
                ("C-c C-k" . haskell-interactive-mode-clear)
                :map haskell-cabal-mode-map
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

  (use-package lsp-mode :defer t)
  (use-package lsp-ui :defer t)
  (use-package lsp-haskell
    :defer t

    :init
    (add-hook 'haskell-mode-hook #'lsp)
    (add-hook 'haskell-literate-mode-hook #'lsp))

;  (use-package toml-mode)

  (use-package web-mode
    :mode ("\\.html?\\'"
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.[agj]sp\\'"
           "\\.as[cp]x\\'"
           "\\.erb\\'"
           "\\.mustache\\'"
           "\\.djhtml\\'"))

  (use-package paredit
    ;; Add paredit to lisp modes
    :hook ((cider-repl-mode . paredit-mode)
           (lisp-mode . paredit-mode)
           (emacs-lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode)
           (ielm-mode . paredit-mode)
           (json-mode . paredit-mode)))

  (use-package powershell :defer t))
(add-hook 'after-init-hook #'my-after-init-function)

(defun call-process-string (program &rest args)
  "Call process`PROGRAM' with `ARGS' and return the output as string."
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (buffer-string)))

;; Use simple dired by default
(defun dired-hook-configuration ()
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook #'dired-hook-configuration)

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

;; When prefixing C-k with a number, kill the whole line. The standard behavior
;; is to kill from the point position.
;;
;; Macro and bindings taken from
;; http://endlessparentheses.com/kill-entire-line-with-prefix-argument.html
(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument. The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(global-set-key [remap paredit-kill] (bol-with-prefix paredit-kill))
(global-set-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(global-set-key [remap kill-line] (bol-with-prefix kill-line))

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
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(calendar-date-style 'iso)
 '(calendar-week-start-day 1)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(debug-on-error nil)
 '(delete-selection-mode t)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-dnd-protocol-alist nil)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-listing-switches "-alh")
 '(doc-view-resolution 300)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(fill-column 78)
 '(flycheck-disabled-checkers '(emacs-lisp-checkdoc))
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-discover-mode t)
 '(global-visual-line-mode nil)
 '(gnus-default-nntp-server "news.gmane.org")
 '(grep-highlight-matches t)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(line-number-display-limit-width 1000000)
 '(lsp-ui-doc-position 'bottom)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-doc-use-webkit t)
 '(lsp-ui-sideline-enable nil)
 '(menu-bar-mode nil)
 '(modus-themes-bold-constructs t)
 '(modus-themes-headings '((t)))
 '(modus-themes-italic-constructs t)
 '(modus-themes-mode-line '3d)
 '(modus-themes-scale-headings t)
 '(modus-themes-slanted-constructs t)
 '(modus-themes-variable-pitch-headings t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(mouse-yank-at-point t)
 '(neo-autorefresh t)
 '(neo-mode-line-type 'none)
 '(neo-theme 'icons)
 '(nxml-slash-auto-complete-flag t)
 '(org-fontify-emphasized-text nil)
 '(org-fontify-whole-heading-line t)
 '(org-startup-folded nil)
 '(org-superstar-leading-bullet "  ")
 '(org-superstar-special-todo-items t)
 '(org-support-shift-select t)
 '(org-use-speed-commands t)
 '(package-archives
   '(("org" . "https://elpa.emacs-china.org/org/")
     ("melpa" . "https://elpa.emacs-china.org/melpa/")
     ("gnu" . "https://elpa.emacs-china.org/gnu/")))
 '(package-selected-packages
   '(org-superstar org-superstar-mode ssh-agency tree-sitter tree-sitter-indent tree-sitter-langs try sudo-edit dired orderless ripgrep embark marginalia selectrum haskell-mode flycheck toml-mode lsp-ui lsp-haskell modus-themes paredit web-mode lsp-mode markdown-mode nhexl-mode yaml-mode company drag-stuff use-package-chords key-chord neotree goto-chg multiple-cursors expand-region which-key page-break-lines magit beginend discover doom-modeline use-package))
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-conservatively 2)
 '(scroll-preserve-screen-position 1)
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
 '(wdired-use-dired-vertical-movement 'sometimes)
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
 '(winner-mode t)
 '(xterm-mouse-mode nil))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 130 :width normal :foundry "1ASC" :family "Liberation Mono")))))
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

