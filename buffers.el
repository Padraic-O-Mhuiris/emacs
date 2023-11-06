;;; buffers.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

(defvar pm/fallback-buffer-name "*scratch*")

(defvar-local pm/real-buffer-p nil
  "If non-nil, this buffer should be considered real no matter what. See
`doom-real-buffer-p' for more information.")

(defvar pm/real-buffer-functions
  '(pm/dired-buffer-p)
  "A list of predicate functions run to determine if a buffer is real, unlike
`doom-unreal-buffer-functions'. They are passed one argument: the buffer to be
tested.

Should any of its function returns non-nil, the rest of the functions are
ignored and the buffer is considered real.

See `doom-real-buffer-p' for more information.")

(defvar pm/unreal-buffer-functions
  '(minibufferp pm/special-buffer-p pm/non-file-visiting-buffer-p)
  "A list of predicate functions run to determine if a buffer is *not* real,
unlike `doom-real-buffer-functions'. They are passed one argument: the buffer to
be tested.

Should any of these functions return non-nil, the rest of the functions are
ignored and the buffer is considered unreal.

See `doom-real-buffer-p' for more information.")

(defcustom pm/switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defun pm/run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'pm/switch-buffer-hook)))

(defun pm--message-or-count (interactive message count)
  (if interactive
      (message message count)
    count))

(defun pm/fallback-buffer ()
  "Returns the fallback buffer, creating it if necessary. By default this is the
scratch buffer. See `pm/fallback-buffer-name' to change this."
  (let (buffer-list-update-hook)
    (get-buffer-create pm/fallback-buffer-name)))

(defun pm/dired-buffer-p (buf)
  "Returns non-nil if BUF is a dired buffer."
  (provided-mode-derived-p (buffer-local-value 'major-mode buf)
                           'dired-mode))

(defun pm/non-file-visiting-buffer-p (buf)
  "Returns non-nil if BUF does not have a value for `buffer-file-name'."
  (not (buffer-file-name buf)))

(defun pm/special-buffer-p (buf)
  "Returns non-nil if BUF's name starts and ends with an *."
  (equal (substring (buffer-name buf) 0 1) "*"))

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

(defun pm/temp-buffer-p (buf)
  "Returns non-nil if BUF is temporary."
  (equal (substring (buffer-name buf) 0 1) " "))

(defun pm/real-buffer-p (buffer-or-name)
  "Returns t if BUFFER-OR-NAME is a 'real' buffer.

A real buffer is a useful buffer; a first class citizen in Doom. Real ones
should get special treatment, because we will be spending most of our time in
them. Unreal ones should be low-profile and easy to cast aside, so we can focus
on real ones.

The exact criteria for a real buffer is:

  1. A non-nil value for the buffer-local value of the `doom-real-buffer-p'
     variable OR
  2. Any function in `doom-real-buffer-functions' returns non-nil OR
  3. None of the functions in `doom-unreal-buffer-functions' must return
     non-nil.

If BUFFER-OR-NAME is omitted or nil, the current buffer is tested."
  (or (bufferp buffer-or-name)
      (stringp buffer-or-name)
      (signal 'wrong-type-argument (list '(bufferp stringp) buffer-or-name)))
  (when-let (buf (get-buffer buffer-or-name))
    (when-let (basebuf (buffer-base-buffer buf))
      (setq buf basebuf))
    (and (buffer-live-p buf)
         (not (pm/temp-buffer-p buf))
         (or (buffer-local-value 'pm/real-buffer-p buf)
             (run-hook-with-args-until-success 'pm/real-buffer-functions buf)
             (not (run-hook-with-args-until-success 'pm/unreal-buffer-functions buf))))))

(defun pm/unreal-buffer-p (buffer-or-name)
  "Return t if BUFFER-OR-NAME is an 'unreal' buffer.

See `doom-real-buffer-p' for details on what that means."
  (not (pm/real-buffer-p buffer-or-name)))

(defun pm/buffer-frame-predicate (buf)
  "To be used as the default frame buffer-predicate parameter. Returns nil if
BUF should be skipped over by functions like `next-buffer' and `other-buffer'."
  (or (pm/real-buffer-p buf)
      (eq buf (pm/fallback-buffer))))

;; Prevent modified buffers from auto-saving once a buffer is killed
(defun pm/suppress-save-buffer-query-function ()
  (set-buffer-modified-p nil)
  t) ; Return t so other functions in kill-buffer-query-functions get called.

(add-to-list 'kill-buffer-query-functions 'pm/suppress-save-buffer-query-function)

;; Set initial buffer on startup
;; (setq initial-buffer-choice "~/.config/emacs/init.el")

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

(add-hook 'window-buffer-change-functions #'pm/run-switch-buffer-hooks-h)
(add-hook 'server-visit-hook #'pm/run-switch-buffer-hooks-h)

(provide 'buffers.el)
