;;; ide.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

(setq display-line-numbers-width-start t)

(add-hook 'prog-mode-hook 'smartparens-mode)

(provide 'ide.el)

