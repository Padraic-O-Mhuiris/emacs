;;; projects.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; projectile.el

(projectile-mode +1)

(add-to-list 'projectile-globally-ignored-directories "/nix/*")

(setq projectile-project-search-path
      '(
        "~/.config/emacs"
        "~/notes"
        ("~/code" . 4)))


(pm/leader
  "p" '(:ignore t :which-key "switch project")
  "pp" '(projectile-switch-project :which-key "switch project")
  "pf" '(projectile-find-file :which-key "find project file")
  "pb" '(projectile-switch-to-buffer :which-key "find project buffer")
  ;; ... add other projectile-specific bindings as needed
  )

(setq projectile-sort-order 'recentf)
(setq projectile-per-project-compilation-buffer t)

;; persp-mode.el

(persp-mode 1)

(provide 'projects.el)

