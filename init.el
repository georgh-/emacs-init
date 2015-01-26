;;; This file is public domain

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (concat temporary-file-directory "emacs"))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

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

    (prepend-directory-to-path (concat home "Software/pt/"))
    (prepend-directory-to-path (concat home "Software/cygwin/bin/"))
    (prepend-directory-to-path (concat home "Software/Git/bin/"))))


(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it.
Uses `copy-region-as-kill'."
  (interactive)
  (copy-region-as-kill (point-min) (point-max))
  (message "Buffer content saved to kill ring."))

(global-set-key (kbd "C-x w") 'copy-buffer-as-kill)

;; Use meta and arrows to move between windows. ← → ↑ ↓
(windmove-default-keybindings 'meta)

;; To copy a line without killing it, including newline char (+1)
(defun my-kill-save-line (lines)
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (1+ (line-end-position lines)))
  (message "Saved line to kill-ring"))

(global-set-key (kbd "M-k") 'my-kill-save-line)

;; Packages' configuration.
;;
;; This setup assures packages are loaded after the init file is processed.  It
;; could be done before, but this way packages can also be configured using
;; customize.
(add-hook 'after-init-hook 'my-packages-configuration)
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

  (use-package expand-region
    :ensure
    :bind ("C-=" . er/expand-region))

  (use-package multiple-cursors
    :ensure
    :bind (("C-`" . mc/edit-lines)
	   ("C->" . mc/mark-next-like-this)
	   ("C-<" . mc/mark-previous-like-this)
	   ("C-c C-<" . mc/mark-all-like-this)))

  (use-package adaptive-wrap
    :ensure
    :config (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))

  (use-package ace-jump-mode
    ;; ace-jump: press C-, then the letter to jump to, and it
    ;; highlights the possible alternatives.
    :ensure
    :bind ("C-," . ace-jump-mode))

  (use-package ace-jump-zap
    ;; same as ace-jump but deletes any text inbetween
    :ensure
    :bind ("C-." . ace-jump-zap-up-to-char))

  (use-package paredit
    :ensure
    :diminish paredit-mode
    :config
    ;; Add paredit to lisp modes
    (mapc (lambda (hook) (add-hook hook 'enable-paredit-mode))
	  '(clojure-mode-hook
	    cider-repl-mode-hook
	    lisp-mode-hook
	    emacs-lisp-mode-hook
	    lisp-interaction-mode-hook
	    ielm-mode-hook
	    json-mode-hook)))

  (use-package powershell-mode :ensure :defer)
  (use-package php-mode :ensure :defer)

  (use-package guide-key
    :ensure
    :diminish guide-key-mode
    :init (guide-key-mode 1))

  (use-package js2-mode
    :ensure
    :mode ("\\.js\\'" . js2-mode))

  (use-package undo-tree
    :disabled t
    :diminish undo-tree-mode
    :config (global-undo-tree-mode))

  (use-package magit
    :ensure
    :defer
    :diminish magit-auto-revert-mode)

  (use-package browse-kill-ring
    :ensure
    :config (browse-kill-ring-default-keybindings))

  (use-package diminish
    :ensure
    :config
    (eval-after-load "view" '(diminish 'view-mode))))

(defun custom-dired-keys ()
  (define-key dired-mode-map (kbd "RET") 'dired-view-file))
(add-hook 'dired-mode-hook 'custom-dired-keys)

;; Kill other buffer and window, merge C-x o (change window) and C-x 4 0 (kill
;; current buffer and window)
;; Useful to get rid of help buffers when using two windows (emacs window term)
(defun kill-other-buffer-and-window ()
  "Kills other buffer and its window."
  (interactive)
  (other-window 1)
  (kill-buffer-and-window))

(global-set-key (kbd "C-x 4 o") 'kill-other-buffer-and-window)

(defun kill-current-buffer ()
  "Kills current buffer, does not ask which buffer to kill."
  (interactive)
  (kill-buffer (buffer-name)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(when (system-mac-p)
  (setq ns-command-modifier 'control)
  (setq ns-control-modifier 'alt)
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

(add-hook 'after-make-frame-functions 'enable-or-disable-menu-bar-mode)
(add-hook 'after-init-hook 'enable-or-disable-menu-bar-mode)


;; Default face
;; Use the first font available and ignore all the rest.
;; If any of these are not available, do not change anything.
(defun add-default-font (font-name)
  (when (and (member font-name (font-family-list))
	     (not (assoc 'font initial-frame-alist)))
    (add-to-list 'initial-frame-alist `(font . ,font-name))
    (add-to-list 'default-frame-alist `(font . ,font-name))))

(add-default-font "DejaVu Sans Mono")
(add-default-font "Menlo")
(add-default-font "Consolas")


;; Horizontal scrolling
;; Note that it is the reverse of emacs default keys, which are rebound here to
;; avoid mistakes.
(defvar horizontal-scroll-columns 4)

(defun scroll-left-columns ()
  (interactive)
  (scroll-left horizontal-scroll-columns))

(defun scroll-right-columns ()
  (interactive)
  (scroll-right horizontal-scroll-columns))

(global-set-key (kbd "C-M-,") 'scroll-left-columns)
(global-set-key (kbd "C-M-.") 'scroll-right-columns)
(global-set-key (kbd "C-x >") 'scroll-left)
(global-set-key (kbd "C-x <") 'scroll-right)


(defun eval-last-sexp-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c C-e") 'eval-last-sexp-and-replace)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bidi-paragraph-direction (quote left-to-right))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(browse-kill-ring-display-duplicates nil)
 '(browse-kill-ring-highlight-inserted-item (quote solid))
 '(browse-kill-ring-resize-window nil)
 '(browse-kill-ring-separator "───")
 '(browse-kill-ring-separator-face (quote message-header-xheader))
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(delete-selection-mode t)
 '(fill-column 79)
 '(fringe-mode (quote (4 . 4)) nil (fringe))
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence
   (quote
    ("C-x r" "C-x 4" "C-x 5" "C-x 6" "C-x 8" "C-x a" "C-x n" "C-x v" "C-x RET" "C-x C-k" "C-c" "M-s" "M-g")))
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(help-window-select t)
 '(hscroll-step 1)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-margin 2)
 '(show-paren-delay 0.01)
 '(show-paren-mode t)
 '(temp-buffer-resize-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(view-read-only t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:foreground "blue" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
