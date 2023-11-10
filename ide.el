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

(setq lsp-enable-suggest-server-download nil)
(setq lsp-auto-configure nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-eldoc-hook nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-idle-delay 0.6)
(setq lsp-inlay-hint-enable t)
(setq lsp-log-io nil)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-doc-delay 0.5)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-delay 0.5)

(setq lsp-ui-peek-enable t)
(setq lsp-ui-peek-show-directory t)
(setq lsp-ui-peek-always-show t)

(provide 'ide.el)

