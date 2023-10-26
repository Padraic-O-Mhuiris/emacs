;;; keys.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Assign SPC as the global leader key
(general-create-definer pm/leader
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

;; which-key provides user-feedback for keybind combinations
(which-key-mode)
(setq which-key-idle-delay 0)

;; ESC should work as quit
(general-define-key
 "<escape>" 'keyboard-escape-quit)

;; Some global functionality
(pm/leader
  "r" '(pm/reload-config :which-key "Reload config")
  "u" '(:ignore t :which-key "ui")
  "ut" '(counsel-load-theme :which-key "Select Theme"))

(provide 'keys.el)
