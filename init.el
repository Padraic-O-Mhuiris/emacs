;;; init.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; These might need to be defined before evil is required

;; Requires to be initialised prior to evil being required
;; https://github.com/emacs-evil/evil-collection/issues/60
(setq evil-want-keybinding nil)
(setq evil-want-integration t)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-undo-system 'undo-tree)

(require 'savehist)
(require 'no-littering)
(require 'general)
(require 'which-key)
(require 'doom-themes)
(require 'all-the-icons)
(require 'doom-modeline)
(require 'winner)
(require 'ace-window)
(require 'so-long)
(require 'visual-fill-column)
(require 'evil)
(require 'evil-collection)
(require 'undo-tree)
(require 'magit)
(require 'org)
(require 'org-bullets)
(require 'org-tempo)
(require 'time-stamp)
(require 'org-cliplink)
(require 'org-expiry)
(require 'org-roam)
(require 'ts)
(require 'projectile)
(require 'vertico)
(require 'corfu)
(require 'orderless)
(require 'consult)
(require 'consult-projectile)
(require 'consult-org-roam)
(require 'consult-notes)
(require 'consult-dir)
(require 'marginalia)
(require 'embark)
(require 'embark-consult)
(require 'highlight-numbers)
(require 'rainbow-delimiters)
(require 'smartparens)
(require 'helpful)
(require 'nix-mode)
(require 'persp-mode)
(require 'bufler)
(require 'treesit)
(require 'tree-sitter-langs)
(require 'lsp-mode)
(require 'lsp-ui)
(require 'consult-lsp)
(require 'rustic)
(require 'flycheck)
(require 'recentf)
(require 'ox-hugo)

;; Add an initialization flag which can be used for functionality I don't want to re-execute when the config is reloaded
(defvar pm/initialized nil)
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq pm/initialized t)))

(load-file (concat user-emacs-directory "functions.el"))
(load-file (concat user-emacs-directory "startup.el"))
(load-file (concat user-emacs-directory "keys.el"))
(load-file (concat user-emacs-directory "global-settings.el"))
(load-file (concat user-emacs-directory "minibuffer.el"))
(load-file (concat user-emacs-directory "buffers.el"))
(load-file (concat user-emacs-directory "ui.el"))
(load-file (concat user-emacs-directory "history.el"))
(load-file (concat user-emacs-directory "editing.el"))
(load-file (concat user-emacs-directory "projects.el"))
(load-file (concat user-emacs-directory "window.el"))
(load-file (concat user-emacs-directory "completion.el"))
(load-file (concat user-emacs-directory "vc.el"))
(load-file (concat user-emacs-directory "org.el"))
(load-file (concat user-emacs-directory "notes/notes.el"))
(load-file (concat user-emacs-directory "ide.el"))
(load-file (concat user-emacs-directory "utils.el"))
(load-file (concat user-emacs-directory "languages/nix.el"))

;; Set persp-mode to last ensures initialisation process is finalised
;; (persp-mode 1)

(provide 'init)
;;; init.el ends here


