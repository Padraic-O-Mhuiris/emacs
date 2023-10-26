;;; minibuffer.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Minibuffer performance should not be mitigated by garbage collection
(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold (* pm/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold pm/gc-cons-threshold)))

;; Allow yes/no decisions to be answered with y/n
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'minibuffer.el)

