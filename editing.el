;;; editing.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Assign evil mode keybinds using general
(general-evil-setup t)

;; Tabs vs Spaces -> Use spaces when tabbing
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
;; Balances behavior between completion and indentation dependent on context
(setq-default tab-always-indent 'complete)

;; These might need to be defined before evil is required
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-undo-system 'undo-tree)

;; Use evil-mode
(evil-mode 1)

;; Sane defaults for a bunch of collected major modes
(evil-collection-init)

;; Ensure initial states in certain buffers use evil in normal mode
(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;; Respect emacs quit when editing
(general-def 'evil-insert-state-map
  "C-g" 'evil-normal-state
  "C-h" 'evil-delete-backward-char-and-join)

;; Not sure if these are necessary? 
(general-define-key 
 :states 'motion
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line)



(provide 'editing.el)
