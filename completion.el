;;; completion.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;;; vertico

(vertico-mode)
(setq vertico-scroll-margin 0)
(setq vertico-count 20)
(setq vertico-resize t)
(setq vertico-cycle t)

;;; corfu
(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)
(setq completion-cycle-threshold 8)
(setq tab-always-indent 'complete)

;;; orderless
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

;;; consult

;;; marginalia
(marginalia-mode)

;;; embark
(general-define-key
 "C-." 'embark-act
 "C-;" 'embark-dwim
 "C-h B" 'embark-bindings)

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)


(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(provide 'completion.el)

