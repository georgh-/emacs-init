;;; -*- lexical-binding: t -*-

(setq system-windows? (string-equal system-type "windows-nt"))
(setq system-mac?     (string-equal system-type "darwin"))
(setq system-android? (string-equal system-type "android"))
(setq system-gnu?     (not (or system-mac?
                               system-windows?
                               system-android?)))
