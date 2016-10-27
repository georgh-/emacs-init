;;; This file is public domain

(setq ring-bell-function #'ignore)
(defalias 'yes-or-no-p #'y-or-n-p)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (concat temporary-file-directory "emacs-" user-login-name))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix (concat emacs-tmp-dir
										 "/auto-save-list/saves-"))

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

;; Open as root using Tramp. Idea based on
;; http://emacs-fu.blogspot.com.br/2013/03/editing-with-root-privileges-once-more.html
(defun find-file-as-root ()
  "Like `ido-find-file', but automatically edit the file with
root-privileges (using tramp/su), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/su:root@localhost:" file)))
    (find-file file)))

(defun dired-as-root ()
  "Like `ido-dired', but automatically edit the directory with
root-privileges (using tramp/su)."
  (interactive)
  (let ((directory (ido-read-directory-name "Dired as root: ")))
    (setq directory (concat "/su:root@localhost:" directory))
    (dired directory)))

(global-set-key (kbd "C-x F") #'find-file-as-root)
(global-set-key (kbd "C-x D") #'dired-as-root)

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

;; Use meta and arrows to move between windows. ← → ↑ ↓
;;(windmove-default-keybindings 'meta)

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

  (use-package smartscan
	:ensure
	:config (global-smartscan-mode 1))

  (use-package discover
	:ensure)

  (use-package expand-region
    :ensure
    :bind ("C-=" . er/expand-region))

  (use-package multiple-cursors
    :ensure
    :bind (("C->" . mc/mark-next-like-this)
		   ("C-<" . mc/mark-previous-like-this)
		   ("C-c C->" . mc/mark-all-like-this)))

  (use-package adaptive-wrap
    :ensure
    :config (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

  (use-package goto-chg
    :ensure
    :bind (("C-," . goto-last-change)
		   ("C-." . goto-last-change-reverse)))

  (use-package paredit
    :ensure
    :diminish paredit-mode
    :config
    ;; Add paredit to lisp modes
    (mapc (lambda (hook) (add-hook hook #'enable-paredit-mode))
		  '(clojure-mode-hook
			cider-repl-mode-hook
			lisp-mode-hook
			emacs-lisp-mode-hook
			lisp-interaction-mode-hook
			ielm-mode-hook
			json-mode-hook)))

  (use-package powershell :ensure :defer)
  (use-package php-mode :ensure :defer)

  (use-package guide-key
    :ensure
    :diminish guide-key-mode
    :config (guide-key-mode 1))

  (use-package js2-mode
    :ensure
    :mode ("\\.js\\'" . js2-mode))

  (use-package magit
    :ensure
    :defer
    :diminish magit-auto-revert-mode
	:bind ("<f10>" . magit-status))

  (use-package browse-kill-ring
    :ensure
    :config (browse-kill-ring-default-keybindings))

  (use-package page-break-lines
    :ensure
    :diminish page-break-lines-mode
    :config (global-page-break-lines-mode))

  (use-package diminish
    :ensure
    :config (eval-after-load "view" '(diminish 'view-mode))))

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

(add-hook 'after-make-frame-functions 'enable-or-disable-menu-bar-mode)
(add-hook 'after-init-hook 'enable-or-disable-menu-bar-mode)


;; Configure default face
;; Given a font-size (like "10") and a list of fonts,
;; set the frame default font to the first font in the list that exists in the
;; system.
;; If any of these are not available, do not change anything.
;;
;; It's slightly convoluted because I need a different font size depending on
;; the system but I would like to chose a font depending on what's available.
;; For example, if the system has DejaVu sans, use it; otherwise something else.
(defun add-default-font-list (font-size font-name-list)
  (cl-flet
      ((add-default-font
		(font-name)
		(when (and (member font-name (font-family-list))
				   (not (assoc 'font initial-frame-alist)))

		  (let ((font-with-size (concat font-name "-" font-size)))
			(add-to-list 'initial-frame-alist `(font . ,font-with-size))
			(add-to-list 'default-frame-alist `(font . ,font-with-size))))))

    (mapc #'add-default-font font-name-list)))

(add-default-font-list (cond ((system-mac-p) "11")
							 (t "9"))
					   '("DejaVu Sans Mono"
						 "Menlo"
						 "Consolas"))

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

(global-set-key (kbd "C-M-,") #'scroll-left-columns)
(global-set-key (kbd "C-M-.") #'scroll-right-columns)
(global-set-key (kbd "C-x >") #'scroll-left)
(global-set-key (kbd "C-x <") #'scroll-right)


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


;; From http://blog.bookworm.at/2007/03/pretty-print-xml-with-emacs.html
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. Only works in XML/HTML
modes. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end))
  (message "Indented XML."))

(defun pretty-print-xml-buffer ()
  (interactive)
  (pretty-print-xml-region (point-min) (point-max)))

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
    (beginning-of-buffer)
    (while (re-search-forward "[ \\t
]+" nil t)
      (replace-match " or " nil nil))

    ;; Replace any " or " left at the end of the line
    (beginning-of-buffer)
    (while (re-search-forward " or $" nil t)
      (replace-match "" nil nil))

    ;; Copy the text to the system clipboard
    (clipboard-kill-region (point-min) (point-max))
    (pop kill-ring)

    (message "Clipboard IDs have been formatted for HPQC.")))

(global-set-key (kbd "C-c q") #'prepare-clipboard-list-ids-for-hpqc)

(unless (system-windows-p)
  (load-theme 'adwaita t))

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
 '(delete-selection-mode t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 79)
 '(global-discover-mode t)
 '(global-visual-line-mode t)
 '(grep-highlight-matches t)
 '(guide-key-mode t)
 '(guide-key/guide-key-sequence
   (quote
	("C-c" "C-x 4" "C-x 5" "C-x 6" "C-x 8" "C-x C-k" "C-x ESC" "C-x RET" "C-x a" "C-x n" "C-x r" "C-x v" "C-x w" "ESC" "M-g" "M-o" "M-s" "%" "*" "/" "s" "C-h")))
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(help-window-select t)
 '(hscroll-step 1)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(ido-use-filename-at-point (quote guess))
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(nxml-slash-auto-complete-flag t)
 '(org-use-speed-commands t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/"))))
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
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(view-read-only t)
 '(visual-line-fringe-indicators (quote (left-curly-arrow right-curly-arrow)))
 '(wdired-use-dired-vertical-movement (quote sometimes))
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:foreground "grey75"))))
 '(show-paren-match ((t (:foreground "blue" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
