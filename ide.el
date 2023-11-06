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
(add-hook 'prog-mode-hook 'smartparens-mode)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

(setq display-line-numbers-width-start t)

(envrc-global-mode)

(setq lsp-enable-symbol-highlighting nil)
(setq lsp-eldoc-hook nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-idle-delay 0.6)
(setq lsp-inlay-hint-enable t)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (setq lsp-ui-peek-always-show t)
;; (setq lsp-ui-sideline-show-hover t)
;; (setq lsp-ui-doc-enable nil)

(provide 'ide.el)

