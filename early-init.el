;;; -*- lexical-binding: t -*-

(setq system-windows? (string-equal system-type "windows-nt"))
(setq system-mac?     (string-equal system-type "darwin"))
(setq system-android? (string-equal system-type "android"))
(setq system-gnu?     (not (or system-mac?
                               system-windows?
                               system-android?)))

;; Fonts

(setq monospace-font
      (cond (system-gnu? "NotoMono NF")
            (system-windows? "Cascadia Code NF SemiLight 11")))

(setq proportional-font
      (cond (system-gnu? "Noto Sans")
            (system-windows? "Segoe UI 12")))

(when (boundp 'monospace-font)
  (defun set-fonts (f)
    (set-face-font 'default monospace-font)
    (set-face-font 'fixed-pitch monospace-font)
    (set-face-font 'variable-pitch proportional-font))

  (add-hook 'after-make-frame-functions #'set-fonts))


