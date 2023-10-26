;;; window.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; The fringe is the narrow vertical areas on either side of windows
(set-fringe-mode 0)

;; Useful mode to undo and redo window configurations
;; C-c <left> reverts to previous window configuration
;; C-c <right> goes forward in the window configuration undo history
(winner-mode t)

;; Set ace-window labels for jumping between frames
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(provide 'window.el)
