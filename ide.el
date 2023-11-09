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

;; ;; Temporarily set lsp-keymap-prefix
;; (setq lsp-keymap-prefix "s-l") ; `s-l` is an example, can be anything rarely used
;; (with-eval-after-load 'lsp-mode
;;   (general-create-definer pm/lsp-leader-def
;;     :prefix "SPC l")

;;   ;; Bind all the keys from lsp-command-map under `SPC l`
;;   (pm/lsp-leader-def
;;    :keymaps 'lsp-command-map
;;    :states '(normal insert visual emacs)
;;    "" nil ; unbind the default lsp keymap binding
;;    :wk-full-keys nil
;;    :wk-ignore-keys '("s-l")
;;    :wk-prevent-collision t
;;    :wk-prevent-default t
;;    :wk-match-keys 'lsp-command-map
;;    :definer 'minor-mode
;;    :minor-mode 'lsp-mode
;;    "SPC l" lsp-command-map))

;; ;; Use this to ensure the keybinding descriptions are visible in which-key
;; (with-eval-after-load 'which-key
;;   (push '(("SPC l" . "lsp-command-map")) which-key-replacement-alist))

(provide 'ide.el)

