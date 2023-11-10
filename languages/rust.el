;;; rust.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:
(add-to-list 'projectile-project-root-files "Cargo.toml")

(setq rustic-format-on-save t)
(setq rustic-indent-method-chain t)
(setq rustic-lsp-setup-p t)

(setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
(setq lsp-rust-analyzer-display-chaining-hints t)
(setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
(setq lsp-rust-analyzer-display-closure-return-type-hints t)
(setq lsp-rust-analyzer-display-parameter-hints nil)
(setq lsp-rust-analyzer-display-reborrow-hints nil)
(setq lsp-rust-analyzer-cargo-watch-command "clippy")

;; (add-to-list 'flycheck-checkers 'rustic-clippy)

(add-hook 'after-init-hook
          #'(lambda ()
              (push 'rustic-clippy flycheck-checkers)))

(provide 'rust.el)

