;;; buffers.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Prevent modified buffers from auto-saving once a buffer is killed
(defun pm/suppress-save-buffer-query-function ()
  (set-buffer-modified-p nil)
  t) ; Return t so other functions in kill-buffer-query-functions get called.

(add-to-list 'kill-buffer-query-functions 'pm/suppress-save-buffer-query-function)

;; Set initial buffer on startup
;; (setq initial-buffer-choice "~/.config/emacs/init.el")

;; Scratch buffer should always be blank
(setq initial-scratch-message nil)

;; Respect file mutation if external change occurs to a file
(global-auto-revert-mode t)
(setq auto-revert-interval 1)

;; Always prefer utf-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Mitigates issues where line length in buffers is enormous
(add-hook 'after-init-hook 'global-so-long-mode)

;; Sensible defaults for global line wrapping behaviour in all text and program modes. Lines will "wrap" at column 140 but will respect window width first
(setq fill-column 80)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'visual-line-mode-hook
          #'(lambda ()
              (setq visual-fill-column-width 140) 
              (visual-fill-column-mode)))


(bufler-mode)

(general-define-key
 "C-x b" 'bufler-switch-buffer)

(pm/leader
  "b" '(:ignore t :which-key "buffers")
  "bb" '(bufler-switch-buffer :which-key "switch buffer")
  "bl" '(bufler-list :which-key "display buffer list")
  ;; ... add other projectile-specific bindings as needed
  )



(provide 'buffers.el)
