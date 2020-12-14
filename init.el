;;; This file is public domain

(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(defalias 'yes-or-no-p #'y-or-n-p)

(when (fboundp 'native-compile-async)
  (setq comp-deferred-compilation t
        comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))

(when (eq window-system 'pgtk)
  (pgtk-use-im-context t))

;; Required in Windows 7, otherwise it uses iso-8859-15
(set-language-environment "utf-8")

;; Add a hook function to multiple mode hooks
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

  ;;; General

  (use-package modus-themes
    :config (modus-themes-load-operandi))

  (use-package doom-modeline
    :defer nil
    :config
    (setq doom-modeline-buffer-file-name-style 'relative-to-project)
    (doom-modeline-mode t))
  
  ;; Completion for minibuffer commands
  (use-package ivy
    :config (ivy-mode 1))

  ;; Used to show extra commands during ivy completion M-o
  (use-package ivy-hydra)

  ;; Extends ivy options M-o
  (use-package counsel
    :config (counsel-mode 1))

  ;; M-n and M-p go to next or previous symbol matching symbol under cursor
  (use-package smartscan
    :config (global-smartscan-mode 1))

  ;; Shows help for some commands
  (use-package discover)

  ;; Provides history order to counsel-M-x
  (use-package smex)

  ;; Smarter placement of cursor at begining of buffer M-< M->
  (use-package beginend
    :config

    ;; Add beginend for all supported modes
    (beginend-setup-all))

  (use-package magit
    :bind ("<f10>" . #'magit-status))

  (use-package browse-kill-ring
    :config (browse-kill-ring-default-keybindings))

  ;; Show a horizontal line instead of ^L character (new page character)
  ;; May have bad interactions with adaptive-wrap
  (use-package page-break-lines
    :config (global-page-break-lines-mode))

  (use-package hide-lines
    :defer t
    :bind ("C-c h" . hide-lines))

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
    :defer t
    :bind (("<f9>" . neotree-show)
           (:map neotree-mode-map ("<f9>" . neotree-hide))))
  
  (unless (system-windows-p)
    (use-package pdf-tools :defer t))

;;;Keyboard

  (use-package key-chord
    :defer nil
    :config
    (key-chord-define-global "xk" #'kill-current-buffer)
    (key-chord-define-global "x0" #'delete-window)
    (key-chord-define-global "x1" #'delete-other-windows)
    (key-chord-define-global "x2" #'split-window-below)
    (key-chord-define-global "x3" #'split-window-right)
    (key-chord-define-global "xb" #'ivy-switch-buffer)
    (key-chord-define-global "kp" #'previous-buffer)
    (key-chord-define-global "km" #'next-buffer)
    (key-chord-define-global "xf" #'counsel-find-file)
    (key-chord-define-global "xs" #'save-buffer)
    (key-chord-define-global "xw" #'write-file))

  (use-package use-package-chords
    :defer nil
    :config (key-chord-mode 1))

  (use-package drag-stuff
    :defer nil
    :bind (("M-<up>" . drag-stuff-up)
           ("M-<down>" . drag-stuff-down))
    :config (drag-stuff-global-mode))

  ;; Complete Anything - Code completion framework
  (use-package company
    :config (global-company-mode 1))

  ;; Syntax analyzer (coding modes), spellchecker (non-coding modes)
  ;; (use-package flycheck
  ;;   :hook ((text-mode . flycheck-mode)
  ;;          (prog-mode . flycheck-mode)))

;;;Language modes

  (use-package org-superstar
    :hook (org-mode . org-superstar-mode))

  (use-package yaml-mode :defer t)

  (use-package nhexl-mode :defer t)

  (use-package markdown-mode :defer t)

  (use-package lsp-mode :defer t)

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
  "Kill current buffer, does not ask which buffer to kill."
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


;; Use ido buffer instead of default
(global-set-key (kbd "C-x C-b") #'ibuffer)

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
 '(TeX-engine 'luatex)
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-inserted-item 'solid)
 '(browse-kill-ring-resize-window nil)
 '(browse-kill-ring-separator "───")
 '(browse-kill-ring-separator-face 'message-header-xheader)
 '(calendar-date-style 'iso)
 '(calendar-week-start-day 1)
 '(column-number-indicator-zero-based nil)
 '(column-number-mode t)
 '(counsel-find-file-at-point t)
 '(counsel-root-command "su")
 '(create-lockfiles nil)
 '(cua-enable-cua-keys nil)
 '(custom-safe-themes
   '("77cdb6c4d2bd17e707943d19d58759565dd14eb881f46b4031b382fc0c9ebb0a" "cfa3b266957e26ed5a8637f43d443b4a921bb546381d7df97e7338d278184fa9" default))
 '(debug-on-error nil)
 '(delete-selection-mode t)
 '(dired-auto-revert-buffer 'dired-directory-changed-p)
 '(dired-dnd-protocol-alist nil)
 '(dired-listing-switches "-alh")
 '(doc-view-resolution 300)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elm-format-on-save t)
 '(fill-column 78)
 '(frame-resize-pixelwise t)
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
 '(ivy-mode t)
 '(ivy-on-del-error-function 'ignore)
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(line-number-display-limit-width 1000000)
 '(menu-bar-mode nil)
 '(modus-themes-bold-constructs t)
 '(modus-themes-headings '((t)))
 '(modus-themes-mode-line '3d)
 '(modus-themes-slanted-constructs t)
 '(modus-themes-variable-pitch-headings t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control))))
 '(mouse-yank-at-point t)
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
   '(("org" . "http://elpa.emacs-china.org/org/")
     ("melpa" . "http://elpa.emacs-china.org/melpa/")
     ("gnu" . "http://elpa.emacs-china.org/gnu/")))
 '(package-selected-packages
   '(org-superstar org-superstar-mode modus-themes paredit web-mode lsp-mode markdown-mode nhexl-mode yaml-mode company drag-stuff use-package-chords key-chord pdf-tools neotree macrostep goto-chg multiple-cursors expand-region which-key dired-details hide-lines page-break-lines browse-kill-ring magit beginend smex discover smartscan counsel ivy-hydra ivy doom-modeline use-package))
 '(ring-bell-function 'ignore)
 '(savehist-mode t)
 '(scroll-conservatively 2)
 '(scroll-preserve-screen-position 1)
 '(show-paren-delay 0.001)
 '(show-paren-mode t)
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
 '(winner-mode t)
 '(xterm-mouse-mode t))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:

