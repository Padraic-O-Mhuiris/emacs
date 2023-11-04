;;; buffers.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Prevent modified buffers from auto-saving once a buffer is killed
(defun pm/suppress-save-buffer-query-function ()
  (set-buffer-modified-p nil)
  t) ; Return t so other functions in kill-buffer-query-functions get called.

(add-to-list 'kill-buffer-query-functions 'pm/suppress-save-buffer-query-function)

;; Set initial buffer on startup
(setq initial-buffer-choice "~/.config/emacs/init.el")

;; Scratch buffer should always be blank
(setq initial-scratch-message nil)

;; Respect file mutation if external change occurs to a file
(global-auto-revert-mode t)
(setq auto-revert-interval 1)

;; Always prefer utf-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Mitigates issues where line length in buffers is enormous
(add-hook 'after-init-hook 'global-so-long-mode)

;; Sensible defaults for global line wrapping behaviour in all text and program modes. Lines will "wrap" at column 140 but will respect window width first
(setq fill-column 80)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'visual-line-mode-hook
          #'(lambda ()
              (setq visual-fill-column-width 140) 
              (visual-fill-column-mode)))

(defun pm--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

(defun pm/fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `pm/fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create pm/fallback-buffer-name)))

(defun pm/project-buffer-list (&optional project)
  "Return a list of buffers belonging to the specified PROJECT.

If PROJECT is nil, default to the current project.

If no project is active, return all buffers."
  (let ((buffers (buffer-list)))
    (if-let* ((project-root
               (if project (expand-file-name project)
                 (pm/project-root))))
        (cl-loop for buf in buffers
                 if (projectile-project-buffer-p buf project-root)
                 collect buf)
      buffers)))

(defun pm/kill-all-buffers (&optional buffer-list interactive)
  "Kill all buffers and closes their windows.

If the prefix arg is passed, doesn't close windows and only kill buffers that
belong to the current project."
  (interactive
   (list (if current-prefix-arg
             (pm/project-buffer-list)
           (buffer-list))
         t))
  (if (null buffer-list)
      (message "No buffers to kill")
    (save-some-buffers)
    (delete-other-windows)
    (when (memq (current-buffer) buffer-list)
      (switch-to-buffer (pm/fallback-buffer)))
    (mapc #'kill-buffer buffer-list)
    (pm--message-or-count
     interactive "Killed %d buffers"
     (- (length buffer-list)
        (length (cl-remove-if-not #'buffer-live-p buffer-list))))))


(provide 'buffers.el)
