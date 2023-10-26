;;; global-settings.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Assign user name and email
(setq user-full-name "Patrick H Morris"
      user-mail-address "patrick.morris.310@gmail.com")

;; Customization file not that I will use it
(let ((customization-file
       (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p customization-file)
    (write-region "" nil customization-file))
  (setq custom-file customization-file)
  (load custom-file 'noerror))

;; Warn when opening files > 100MB
(setq large-file-warning-threshold 100000000)

;; Don't make backups
(setq make-backup-files nil)

;; Legacy weird writing thing from typewriters, prevent
(setq sentence-end-double-space nil)

(provide 'global-settings.el)
