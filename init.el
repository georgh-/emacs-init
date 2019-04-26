;;; This file is public domain


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(defalias 'yes-or-no-p #'y-or-n-p)

;; Required in Windows 7, otherwise it uses iso-8859-15
(set-language-environment "utf-8")

;; Add a hook function fto multiple mode hooks
(defun add-to-multiple-hooks (func hooks)
  (mapc (lambda (hook) (add-hook hook func))
        hooks))

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (concat temporary-file-directory "emacs-" user-login-name))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix (concat emacs-tmp-dir
                                         "/auto-save-list/saves-"))

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

;; Add a path to the PATH environment variable and to exec-path emacs variable
(defun prepend-directory-to-path (dir)
  (setenv "PATH" (concat dir path-separator (getenv "PATH")))
  (add-to-list 'exec-path dir))

;; Set path to UNIX utilities in Windows
(when (system-windows-p)
  (let ((home (file-name-as-directory
               (concat (getenv "HOMEDRIVE")
                       (getenv "HOMEPATH")))))

    (prepend-directory-to-path (concat home "Software/Git/bin/"))
    (prepend-directory-to-path (concat home "Software/msys2/usr/bin/"))))

;; Set npm path in Linux
(when (system-unix-p)
  (prepend-directory-to-path "~/bin")
  (prepend-directory-to-path "~/software/node/bin"))

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
  "Kills a line without deleting it. Includes newline character."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-end-position nlines))
  (kill-append "\n" nil)
  (message "Saved line to kill-ring"))

(global-set-key (kbd "M-k") #'kill-save-line)

;; Packages' configuration.
;;
;; This setup assures packages are loaded after the init file is processed.  It
;; could be done before, but this way packages can also be configured using
;; customize.
(add-hook 'after-init-hook #'my-packages-configuration)
(defun my-packages-configuration ()
  (unless (boundp 'package--initialized)
    (message "There are no packages in the system. Initializing package...")
    (package-initialize))

  (unless package-archive-contents
    (message "Initializing and refreshing ELPA package archives...")
    (package-refresh-contents))

  (unless (package-installed-p 'use-package)
    (message "Installing `use-package'...")
    (package-install 'use-package))

  (require 'use-package)

  (use-package projectile :defer t)
  
  (use-package nhexl-mode :defer t)

  (use-package neotree
    :defer t
    :bind (("<f9>" . neotree-show)
           (:map neotree-mode-map ("<f9>" . neotree-hide))))

  (use-package web-mode
    :mode ("\\.html?\\'"
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.[agj]sp\\'"
           "\\.as[cp]x\\'"
           "\\.erb\\'"
           "\\.mustache\\'"
           "\\.djhtml\\'"))

  (use-package doom-modeline
    :defer nil
    :config
    (setq doom-modeline-buffer-file-name-style 'relative-to-project)
    (doom-modeline-mode t))

  ;; Complete Anything - Code completion framework
  (use-package company
    :delight
    :config (global-company-mode 1))

  ;; Syntax analyzer (coding modes), spellchecker (non-coding modes)
  (use-package flycheck
    :hook ((text-mode . flycheck-mode)
           (prog-mode . flycheck-mode)))

  ;; Code analyzer using clang as backend
  (use-package irony
    :delight
    :hook ((c++-mode . irony-mode)
           (c-mode . irony-mode)
           (objc-mode . irony-mode))
    :config (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (use-package company-irony :defer t)
  (use-package irony-eldoc :defer t)
  (use-package flycheck-irony :defer t)

  ;; When enabled keeps the buffer always indented
  (use-package aggressive-indent :defer t)

  ;; Smarter placement of cursor at begining of buffer M-< M->
  (use-package beginend
    :config

    ;; Add beginend for all supported modes
    (beginend-setup-all)

    ;; Delight all modes individually
    (mapc (lambda (pair) (delight (cdr pair) nil 'beginend))
          beginend-modes))
  
  (unless (system-windows-p)
    (use-package pdf-tools :defer t))

  (use-package scss-mode :defer t)

  ;; Keybindings with one key, easy to define. Req. by counsel
  (use-package hydra
    :bind ("C-c f" . hydra-flycheck/body)
    :config
    
    (defhydra hydra-flycheck (:color blue)
      "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
      ("q" nil)
      ("<" flycheck-previous-error)
      (">" flycheck-next-error)
      ("?" flycheck-describe-checker)
      ("M" flycheck-manual)
      ("d" flycheck-disable-checker)
      ("f" flycheck-buffer)
      ("l" flycheck-list-errors)
      ("m" flycheck-mode)
      ("s" flycheck-select-checker)
      ("v" flycheck-verify-setup)))

  ;; Provides history order to counsel-M-x
  (use-package smex)

  ;; Completion for minibuffer commands
  (use-package ivy
    :config (ivy-mode 1)
    :delight)

  ;; Used to show extra commands during ivy completion M-o
  (use-package ivy-hydra)

  ;; Extends ivy options M-o
  (use-package counsel
    :config (counsel-mode 1)
    :delight)

  ;; M-n and M-p go to next or previous symbol matching symbol under cursor
  (use-package smartscan
    :config (global-smartscan-mode 1))

  ;; Shows help for some commands
  (use-package discover)

  (use-package expand-region
    :bind ("C-=" . er/expand-region))

  (use-package multiple-cursors
    :bind (("C->" . mc/mark-next-like-this)
           ("C-<" . mc/mark-previous-like-this)
           ("C-c C->" . mc/mark-all-like-this)))

  ;; Improves wrap mode by preserving left margin, comments, etc.
  (use-package adaptive-wrap
    :delight visual-line-mode
    :hook (visual-line-mode . adaptive-wrap-prefix-mode))

  ;; Goes to last changed text in current buffer
  (use-package goto-chg
    :bind (("C-," . goto-last-change)
           ("C-." . goto-last-change-reverse)))

  (use-package macrostep :defer t)

  (use-package paredit
    :delight paredit-mode
    ;; Add paredit to lisp modes
    :hook ((clojure-mode . paredit-mode)
           (cider-repl-mode . paredit-mode)
           (lisp-mode . paredit-mode)
           (emacs-lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode)
           (ielm-mode . paredit-mode)
           (json-mode . paredit-mode)))

  (use-package powershell :defer t)
  (use-package php-mode :defer t)

  ;; Shows key shortcuts and commands while typing a keyboard shortcut
  ;; For example, type C-c and wait, and it will show a guide
  (use-package which-key
    :delight which-key-mode
    :config (which-key-mode 1))

  (use-package hide-lines
    :defer t
    :bind ("C-c h" . hide-lines))

  ;; Enable origami folding mode. JS2-mode uses a better folding by default
  (use-package origami
    :config
    (defun enable-origami-mode-x ()
      (unless (derived-mode-p 'js2-mode)
        (origami-mode 1)))
    :hook (prog-mode . enable-origami-mode-x))

  (use-package js2-mode
    :defer t)

  ;; rjsx-mode extends js2-mode with JSX react extension
  (use-package rjsx-mode
    :defer t
    :mode ("\\.js\\'" . rjsx-mode))

  (use-package js2-refactor
    :defer t
    :delight
    :hook
    (js2-mode . js2-refactor-mode)
    :bind (:map js2-mode-map
                ("C-k" . js2r-kill))
    :config
    (js2r-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'js2-mode-hook
              (lambda ()
                (add-hook 'xref-backend-functions
                          #'xref-js2-xref-backend nil t))))
  
  (use-package xref-js2
    :defer t
    ;; js-mode (which js2 is based on) binds "M-." which conflicts
    ;;with xref, so unbind it.
    :bind (:map js-mode-map ("M-." . nil)))

  (use-package company-tern
    :defer t
    :hook '((js2-mode . tern-mode)
            (js2-mode . company-mode))
    
    ;; Disable completion keybindings, as we use xref-js2 instead
    :bind (:map tern-mode-keymap
                ("M-." . nil)
                ("M-," . nil))
    
    :config (add-to-list 'company-backends 'company-tern))

  (use-package magit
    :delight magit-auto-revert-mode
    :bind ("<f10>" . magit-status))

  (use-package browse-kill-ring
    :config (browse-kill-ring-default-keybindings))

  ;; Show a horizontal line instead of ^L character (new page character)
  ;; May have bad interactions with adaptive-wrap
  (use-package page-break-lines
    :delight page-break-lines-mode
    :config (global-page-break-lines-mode))

  ;; Remove details from dired, toggle with (
  (use-package dired-details)

  ;; configure these dependencies
  (use-package tern :defer t :diminish)
  (use-package yasnippet :defer t :delight (yas-minor-mode))

  ;; Removes mode indicator from modeline, integrated in use-package
  (use-package delight
    :config (delight '((eldoc-mode nil "Eldoc")))))

;; Enable visual-line mode only for programming modes
;; It will stay disabled for any other mode (occur, packages, etc)
(defun enable-visual-line-mode ()
  (visual-line-mode t))
(add-hook 'prog-mode-hook #'enable-visual-line-mode)

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


;; Use ido buffer instead of default
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Command key in Mac keyboard is used as control key in Emacs
(when (system-mac-p)
  (setq ns-command-modifier 'control)
  (setq ns-pop-up-frames nil))

;; Disable menu-bar-mode everywhere, but allow it on OSX graphic interface.
;; Mac OS X always shows a menu bar. When Emacs menubar mode is disabled
;; in OS X, it is still shown but empty. That's why it is better to show
;; all the menu options rather than an empty menu.
;;
;; The optional parameter is needed on after-make-frame-hook
(defun enable-or-disable-menu-bar-mode (&optional frame)
  (if (and (display-graphic-p)
           (system-mac-p))
      (menu-bar-mode 1)
    (menu-bar-mode -1)))

(add-hook 'after-make-frame-functions #'enable-or-disable-menu-bar-mode)
(add-hook 'after-init-hook #'enable-or-disable-menu-bar-mode)

;; Configure default face
(let ((font "DejaVu Sans Mono-10"))
  (add-to-list 'initial-frame-alist `(font . ,font))
  (add-to-list 'default-frame-alist `(font . ,font)))

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
 '(TeX-engine (quote luatex))
 '(bidi-paragraph-direction (quote left-to-right))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-inserted-item (quote solid))
 '(browse-kill-ring-resize-window nil)
 '(browse-kill-ring-separator "───")
 '(browse-kill-ring-separator-face (quote message-header-xheader))
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(counsel-find-file-at-point t)
 '(counsel-root-command "su")
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" default)))
 '(delete-selection-mode t)
 '(dired-dnd-protocol-alist nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 79)
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(global-auto-revert-mode t)
 '(global-discover-mode t)
 '(global-visual-line-mode nil)
 '(gnus-default-nntp-server "news.gmane.org")
 '(grep-highlight-matches t)
 '(help-window-select t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-on-del-error-function (quote ignore))
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2r-always-insert-parens-around-arrow-function-params t)
 '(line-number-display-limit-width 1000000)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(neo-theme (quote icons))
 '(nxml-slash-auto-complete-flag t)
 '(org-use-speed-commands t)
 '(package-archives
   (quote
    (("org" . "https://orgmode.org/elpa/")
     ("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/"))))
 '(package-selected-packages
   (quote
    (neotree doom-mode doom-themes doom-modeline which-key web-mode-edit-element counsel-projectile projectile delight smartparens org-jira org markdown-preview-eww rg diminish dired-details page-break-lines browse-kill-ring magit company-tern xref-js2 js2-refactor rjsx-mode js2-mode origami hide-lines php-mode powershell paredit goto-chg adaptive-wrap multiple-cursors expand-region discover smartscan counsel ivy-hydra ivy smex hydra scss-mode pdf-tools beginend aggressive-indent omnisharp-emacs omnisharp flycheck-irony irony-eldoc company-irony irony flycheck company web-mode nhexl-mode use-package)))
 '(projectile-mode t nil (projectile))
 '(ring-bell-function (quote ignore))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 2)
 '(scroll-margin 2)
 '(scroll-preserve-screen-position 1)
 '(show-paren-delay 0.01)
 '(show-paren-mode t)
 '(tab-width 4)
 '(temp-buffer-resize-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify))
 '(use-package-always-ensure t)
 '(view-read-only t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(which-key-lighter "")
 '(which-key-mode t)
 '(which-key-side-window-max-height 0.6)
 '(whitespace-style
   (quote
    (face tabs spaces newline indentation space-mark tab-mark newline-mark)))
 '(winner-mode t)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:foreground "blue" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
