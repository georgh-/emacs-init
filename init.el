(require 'uniquify)

(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir
  (concat temporary-file-directory "emacs"))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

(defun copy-buffer-as-kill ()
  "Save the buffer as if killed, but don't kill it.
Uses `copy-region-as-kill'."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer content saved to kill ring."))

(global-set-key (kbd "C-x w") 'copy-buffer-as-kill)

;; Use meta and arrows to move between windows. ← → ↑ ↓
(windmove-default-keybindings 'meta)

;; To copy a line without killing it, including newline char (+1)
(defun my-kill-save-line (lines)
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (+ 1 (line-end-position lines)))
  (message "Saved line to kill-ring"))

(global-set-key (kbd "M-k") 'my-kill-save-line)

;; Packages' configuration. This setup achieves 2 goals:
;;  1. Packages are configured upon loading, without requiring them during init.
;;  2. Packages' configuration is done after processing the init file,
;;     which guarantees packages will take configuration from customize.
;;
;; Many users put (requires 'package) directly in .emacs, but then they must
;; not use customize, which I like to use.
;;
;; In this case, use-package must be required in the hook because it is a
;; package installed using package; and package is only initialized after
;; reading the init file. Otherwise it would cause an error.
(add-hook 'after-init-hook 'my-packages-configuration)
(defun my-packages-configuration ()
  (require 'use-package)

  (use-package expand-region
	       :bind ("C-=" . er/expand-region))

  (use-package multiple-cursors
	       :bind (( "C-`" . mc/edit-lines)
		      ( "C->" . mc/mark-next-like-this)
		      ( "C-<" . mc/mark-previous-like-this)
		      ( "C-c C-<" . mc/mark-all-like-this)))

  (use-package ace-jump-mode
	       ;; ace-jump: press C-, then the letter to jump to, and it
	       ;; highlights the possible alternatives.
	       :bind (("C-," . ace-jump-mode)
		      ("C-." . ace-jump-zap-up-to-char))))


;; Kill buffer in other window, merge C-x o (change window) and C-x 4 0 (kill
;; current buffer and window)
;; Useful to get rid of help buffers when using two windows (emacs window term)
;;
;; Possible improvement: advice function kill-buffer-and-window and, if called
;; with a parameter switch to another window before kill-buffer-and-window.
(defun kill-other-buffer-and-window ()
  "Kills other buffer and its window."
  (interactive)
  (other-window 1)
  (kill-buffer-and-window))

(global-set-key (kbd "C-x 4 o 0") 'kill-other-buffer-and-window)

(cond ((string-equal system-type "darwin")
       (setq ns-command-modifier (quote control))
       (setq ns-control-modifier (quote alt))
       (setq ns-pop-up-frames nil)))

(defun enable-or-disable-menu-bar-mode (&optional frame)
  "Disable menu-bar-mode everywhere, but allow it on OSX graphic interface.
Mac OS X always shows a menu bar. When Emacs menubar mode is disabled
in OS X, it is still shown but empty. That's why it is better to show 
all the menu options rather than an empty menu."

  ;; The optional parameter is needed in some invocations of new-frame as part
  ;; of after-make-frame-functions hook

  (interactive)

  (if (and (display-graphic-p) 
	   (string-equal system-type "darwin"))
      (menu-bar-mode 1)
    (menu-bar-mode -1)))

(add-hook 'after-make-frame-functions 'enable-or-disable-menu-bar-mode)
(add-hook 'after-init-hook 'enable-or-disable-menu-bar-mode)

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


(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
(global-set-key (kbd "C-c e") 'fc-eval-and-replace)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bidi-paragraph-direction (quote left-to-right))
 '(blink-cursor-mode nil)
 '(blink-matching-paren nil)
 '(column-number-mode t)
 '(cua-enable-cua-keys nil)
 '(custom-enabled-themes (quote (dichromacy)))
 '(delete-selection-mode t)
 '(fill-column 79)
 '(hscroll-step 1)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(savehist-mode t)
 '(scroll-conservatively 101)
 '(scroll-margin 2)
 '(show-paren-delay 0.01)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "dejavu sans mono"))))
 '(show-paren-match ((t (:foreground "blue" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red" :weight bold)))))
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)
