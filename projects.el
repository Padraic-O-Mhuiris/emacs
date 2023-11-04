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


(setq projectile-sort-order 'recentf)
(setq projectile-per-project-compilation-buffer t)

(pm/leader
  "p" '(:ignore t :which-key "switch project")
  "pp" '(consult-projectile :which-key "switch project")
  "pf" '(consult-projectile-find-file :which-key "find project file")
  "pg" '(consult-grep :which-key "search in project")
  ;; ... add other projectile-specific bindings as needed
  )

(defun pm/project-name (&optional dir)
  "Return the name of the current project.

Returns '-' if not in a valid project."
  (if-let (project-root (or (doom-project-root dir)
                            (if dir (expand-file-name dir))))
      (funcall projectile-project-name-function project-root)
    "-"))

(defun pm/project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defun pm/workspaces-set-project-action-fn ()
  "A `projectile-switch-project-action' that sets the project directory for
`+workspaces-switch-to-project-h'."
  (setq pm/workspaces--project-dir default-directory))

(defun pm/workspaces-switch-to-project-h (&optional dir)
  "Creates a workspace dedicated to a new project. If one already exists, switch
to it. If in the main workspace and it's empty, recycle that workspace, without
renaming it.

Afterwords, runs `+workspaces-switch-project-function'. By default, this prompts
the user to open a file in the new project.

This be hooked to `projectile-after-switch-project-hook'."
  (when dir
    (setq pm/workspaces--project-dir dir))
  ;; HACK Clear projectile-project-root, otherwise cached roots may interfere
  ;;      with project switch (see #3166)
  (let (projectile-project-root)
    (when (and persp-mode pm/workspaces--project-dir)
      (when projectile-before-switch-project-hook
        (with-temp-buffer
          ;; Load the project dir-local variables into the switch buffer, so the
          ;; action can make use of them
          (setq default-directory pm/workspaces--project-dir)
          (hack-dir-local-variables-non-file-buffer)
          (run-hooks 'projectile-before-switch-project-hook)))
      (unwind-protect
          (if (and (not (null pm/workspaces-on-switch-project-behavior))
                   (or (eq pm/workspaces-on-switch-project-behavior t)
                       (equal (safe-persp-name (get-current-persp)) persp-nil-name)
                       (pm/workspace-buffer-list)))
              (let* ((persp
                      (let ((project-name (pm/project-name pm/workspaces--project-dir)))
                        (or (pm/workspace-get project-name t)
                            (pm/workspace-new project-name))))
                     (new-name (persp-name persp)))
                (pm/workspace-switch new-name)
                (with-current-buffer (pm/fallback-buffer)
                  (setq default-directory pm/workspaces--project-dir)
                  (hack-dir-local-variables-non-file-buffer))
                (unless current-prefix-arg
                  (funcall pm/workspaces-switch-project-function pm/workspaces--project-dir))
                (pm/workspace-message
                 (format "Switched to '%s' in new workspace" new-name)
                 'success))
            (with-current-buffer (pm/fallback-buffer)
              (setq default-directory pm/workspaces--project-dir)
              (hack-dir-local-variables-non-file-buffer)
              (message "Switched to '%s'" (pm/project-name pm/workspaces--project-dir)))
            (with-demoted-errors "Workspace error: %s"
              (pm/workspace-rename (pm/workspace-current-name) (pm/project-name pm/workspaces--project-dir)))
            (unless current-prefix-arg
              (funcall pm/workspaces-switch-project-function pm/workspaces--project-dir)))
        (run-hooks 'projectile-after-switch-project-hook)
        (setq pm/workspaces--project-dir nil)))))

(setq projectile-switch-project-action
      (lambda ()
        (pm/workspaces-set-project-action-fn)
        (pm/workspaces-switch-to-project-h)))

      ;; counsel-projectile-switch-project-action
      ;; '(1 ("o" +workspaces-switch-to-project-h "open project in new workspace")
      ;;     ;; ("O" counsel-projectile-switch-project-action "jump to a project buffer or file")
      ;;     ;; ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
      ;;     ;; ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
      ;;     ;; ("D" counsel-projectile-switch-project-action-dired "open project in dired")
      ;;     ;; ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
      ;;     ;; ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
      ;;     ;; ("w" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
      ;;     ;; ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
      ;;     ;; ("r" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
      ;;     ;; ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
      ;;     ;; ("C" counsel-projectile-switch-project-action-configure "run project configure command")
      ;;     ;; ("e" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
      ;;     ;; ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
      ;;     ;; ("s" (lambda (project)
      ;;     ;;        (let ((projectile-switch-project-action
      ;;     ;;               (lambda () (call-interactively #'+ivy/project-search))))
      ;;     ;;          (counsel-projectile-switch-project-by-name project))) "search project")
      ;;     ;; ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
      ;;     ;; ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
      ;;     ;; ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
      ;;     ;; ("X" counsel-projectile-switch-project-action-org-capture "org-capture into project")
      ;;     ))



(provide 'projects.el)

