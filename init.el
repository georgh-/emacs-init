;;; This file is public domain


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(defalias 'yes-or-no-p #'y-or-n-p)

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
    (prepend-directory-to-path (concat home "Software/msys64/usr/bin/"))))

;; Set npm path in Linux
(if (system-unix-p)
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

  (use-package nhexl-mode :defer t)

  (use-package web-mode
    :mode ("\\.html?\\'"
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.[agj]sp\\'"
           "\\.as[cp]x\\'"
           "\\.erb\\'"
           "\\.mustache\\'"
           "\\.djhtml\\'"))

  ;; Complete Anything - Code completion framework
  (use-package company
    :diminish
    :config (global-company-mode 1))

  ;; Syntax analyzer (coding modes), spellchecker (non-coding modes)
  (use-package flycheck
    :hook ((text-mode . flycheck-mode)
           (prog-mode . flycheck-mode)))

  ;; Code analyzer using clang as backend
  (use-package irony
    :diminish
    :hook ((c++-mode . irony-mode)
           (c-mode . irony-mode)
           (objc-mode . irony-mode))
    :config (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

  (use-package company-irony :defer t)
  (use-package irony-eldoc :defer t)
  (use-package flycheck-irony :defer t)

  (use-package omnisharp
    :config (setq omnisharp-server-executable-path
                  (file-name-as-directory
                   (concat  (getenv "HOMEDRIVE")
                            (getenv "HOMEPATH")
                            "/Software/omnisharp")))
    :hook ((csharp-mode . flycheck-mode)
           (csharp-mode . company-omnisharp)))

  (use-package omnisharp-emacs :defer t)
  
  ;; When enabled keeps the buffer always indented
  (use-package aggressive-indent :defer t)

  ;; Smarter placement of cursor at begining of buffer M-< M->
  (use-package beginend
    :diminish beginend-global-mode
    :config
    (beginend-global-mode t)

    ;; Beginend creates minor modes dynamically for each mode it supports
    ;; These modes have to be diminished one by one
    (mapc (lambda (pair)
            (diminish (cdr pair)))
          beginend-modes))

  (unless (system-windows-p)
    (use-package pdf-tools :defer t))

  (use-package scss-mode :defer t)

  ;; Keybindings with one key, easy to define. Req. by counsel
  (use-package hydra)

  ;; Provides history order to counsel-M-x
  (use-package smex)

  ;; Completion for minibuffer commands
  (use-package ivy
    :config (ivy-mode 1)
    :diminish)

  ;; Used to show extra commands during ivy completion M-o
  (use-package ivy-hydra)

  ;; Extends ivy options M-o
  (use-package counsel
    :config (counsel-mode 1)
    :diminish)

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
    :hook (visual-line-mode . adaptive-wrap-prefix-mode))

  ;; Goes to last changed text in current buffer
  (use-package goto-chg
    :bind (("C-," . goto-last-change)
           ("C-." . goto-last-change-reverse)))

  (use-package paredit
    :diminish paredit-mode
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
  (use-package guide-key
    :diminish guide-key-mode
    :config (guide-key-mode 1))

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
    :diminish tern-mode
    :hook '((js2-mode . tern-mode)
            (js2-mode . company-mode))
    
    ;; Disable completion keybindings, as we use xref-js2 instead
    :bind (:map tern-mode-keymap
                ("M-." . nil)
                ("M-," . nil))
    
    :config (add-to-list 'company-backends 'company-tern))

  (use-package magit
    :diminish magit-auto-revert-mode
    :bind ("<f10>" . magit-status))

  (use-package browse-kill-ring
    :config (browse-kill-ring-default-keybindings))

  ;; Show a horizontal line instead of ^L character (new page character)
  ;; May have bad interactions with adaptive-wrap
  (use-package page-break-lines
    :diminish page-break-lines-mode
    :config (global-page-break-lines-mode))

  ;; Remove details from dired, toggle with (
  (use-package dired-details)

  ;; Removes mode indicator from modeline, integrated in use-package
  (use-package diminish
    :config (eval-after-load "view" '(diminish 'view-mode))))

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
(let ((font "DejaVu Sans Mono-8.5"))
  (add-to-list 'initial-frame-alist `(font . ,font))
  (add-to-list 'default-frame-alist `(font . ,font)))

;; Use variable spaced font in configuration and info
(defun enable-variable-pitch-mode ()
    (variable-pitch-mode 1))
(add-hook 'info-mode-hook #'enable-variable-pitch-mode)
(add-hook 'custom-mode-hook #'enable-variable-pitch-mode)
(add-hook 'help-mode-hook #'enable-variable-pitch-mode)

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

(defun prepare-clipboard-list-ids-for-hpqc ()
  "Given a list of ids (all numbers), join them in one line with
\" or \" between them.
The list is always pasted from the clipboard.
Input: 123765\n125816 126953
Output: 123765 or 125816 or 126953"
  (interactive)
  (with-temp-buffer

    ;; Paste system clipboard and leave the kill ring as it was before
    (clipboard-yank)
    (pop kill-ring)

    ;; Replace whitespace by " or "
    (goto-char (point-min))
    (while (re-search-forward "[ \\t
]+" nil t)
      (replace-match " or " nil nil))

    ;; Replace any " or " left at the end of the line
    (goto-char (point-min))
    (while (re-search-forward " or $" nil t)
      (replace-match "" nil nil))

    ;; Copy the text to the system clipboard
    (clipboard-kill-region (point-min) (point-max))
    (pop kill-ring)

    (message "Clipboard IDs have been formatted for HPQC.")))

(global-set-key (kbd "C-c q") #'prepare-clipboard-list-ids-for-hpqc)

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
 '(delete-selection-mode t)
 '(dired-dnd-protocol-alist nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 79)
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc)))
 '(global-discover-mode t)
 '(global-visual-line-mode nil)
 '(gnus-default-nntp-server "news.gmane.org")
 '(grep-highlight-matches t)
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence
   (quote
    ("C-c" "C-x 4" "C-x 5" "C-x 6" "C-x 8" "C-x C-k" "C-x ESC" "C-x RET" "C-x a" "C-x n" "C-x r" "C-x v" "C-x w" "ESC" "M-g" "M-o" "M-s" "%" "*" "/" "s" "C-h")))
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(help-window-select t)
 '(hscroll-step 1)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-selectable-prompt t)
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(js2r-always-insert-parens-around-arrow-function-params t)
 '(line-number-display-limit-width 1000000)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(nxml-slash-auto-complete-flag t)
 '(org-use-speed-commands t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(ring-bell-function (quote ignore))
 '(savehist-mode t)
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
 '(fringe ((t (:foreground "grey75"))))
 '(mode-line ((t (:family "DejaVu Sans"))))
 '(show-paren-match ((t (:foreground "blue" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
