;;; ui.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Remove default emacs ui features
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Show the bell on errors, no noise pls
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; Don't blink cursor
(blink-cursor-mode -1)

;; Make scrolling nice
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Theme
(setq doom-themes-enable-bold t    
      doom-themes-enable-italic t)
(unless pm/initialized (load-theme 'doom-nord-aurora t)) 
(doom-themes-visual-bell-config)
(doom-themes-org-config)

;; Font
(set-face-attribute 'default nil :font "Iosevka Comfy Fixed" :height 100)

;; Modeline
(doom-modeline-mode t)
(setq doom-modeline-height 55)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-line-numbers-style 'relative)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-window-width-limit nil)
;; Ensures modeline always aligns to the rightmost edge
(setq mode-line-right-align-edge 'right-fringe)

(provide 'ui.el)
