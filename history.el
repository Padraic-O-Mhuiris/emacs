;;; history.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

(savehist-mode)

;; undo-tree 
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-show-help-in-visualize-buffer t)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))

;; Assign keybind for using undo-tree
(general-define-key
 "C-x u" 'undo-tree-visualize)


(provide 'history.el)
