;;; utils.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

(if init-file-debug
    (setq warning-minimum-level :debug)
  (setq warning-minimum-level :emergency))

(general-define-key
 "C-h f" 'helpful-function
 "C-h v" 'helpful-variable
 "C-h k" 'helpful-key)

(provide 'utils.el)

