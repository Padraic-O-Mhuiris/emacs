;;; vc.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Some magit settings
(setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; What does this do?

;; Some magit keybinds - to be revised
(pm/leader
  "m" '(:ignore t :which-key "magit")
  "mm" '(magit-status :which-key "status"))


(provide 'vc.el)
