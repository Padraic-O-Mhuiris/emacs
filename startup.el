;;; startup.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Setting this to nil improves performance on startup, the file-name-handler-alist uses lookup keys for file access which can be slow sometimes. This is automatically restored on startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

;; Max out garbage collection thresholds
(defvar pm/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq gc-cons-threshold pm/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

;; Always do native compilation where possible
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

;; Add an initialization flag which can be used for functionality I don't want to re-execute when the config is reloaded
(defvar pm/initialized nil)
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq pm/initialized t)))

;; Don't load the default.el emacs system configuration file
;; NOTE: If this configuration or parts thereof are moved into the nix emacs build, this must be set to nil
(setq inhibit-default-init t)

;; Don't show a startup message by default
(setq inhibit-startup-message t)
(defun display-startup-echo-area-message ()
  (display-startup-time))

(provide 'startup.el)
