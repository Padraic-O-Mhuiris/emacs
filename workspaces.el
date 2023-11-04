;;; workspaces.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

(defvar pm/workspace--last nil)
(defvar pm/workspace--index 0)
(defvar pm/workspaces-main "main")
(defvar pm/fallback-buffer-name "*scratch*")
(defvar pm/workspaces--indirect-buffers-to-restore nil)
(defvar pm/workspace--old-uniquify-style nil)
(defvar pm/workspaces--project-dir nil)
(defvar pm/workspaces-data-file "_workspaces")
(defvar pm/workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a new project.

Can be one of the following:

t           Always create a new workspace for the project
'non-empty  Only create a new workspace if the current one already has buffers
            associated with it.
nil         Never create a new workspace on project switch.")

(defvar pm/workspaces-switch-project-function #'consult-projectile-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one argument: the
new project directory.")

(defalias #'pm/workspace-current #'get-current-persp
  "Return the currently active workspace.")

(defalias #'pm/workspace-p #'perspective-p
  "Return t if OBJ is a perspective hash table.")

(defun pm/workspace--protected-p (name)
  (equal name persp-nil-name))

(defun pm/workspace-list-names ()
  "Return the list of names of open workspaces."
  persp-names-cache)

(defun pm/workspace-current-name ()
  "Get the name of the current workspace."
  (safe-persp-name (pm/workspace-current)))

(defun pm/workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (member name (pm/workspace-list-names)))

(defun pm/workspace-get (name &optional noerror)
  "Return a workspace named NAME. Unless NOERROR is non-nil, this throws an
error if NAME doesn't exist."
  (cl-check-type name string)
  (when-let (persp (persp-get-by-name name))
    (cond ((pm/workspace-p persp) persp)
          ((not noerror)
           (error "No workspace called '%s' was found" name)))))

(defun pm/workspace--generate-id ()
  (or (cl-loop for name in (pm/workspace-list-names)
               when (string-match-p "^#[0-9]+$" name)
               maximize (string-to-number (substring name 1)) into max
               finally return (if max (1+ max)))
      1))

(defun pm/workspace-buffer-list (&optional persp)
  "Return a list of buffers in PERSP.

PERSP can be a string (name of a workspace) or a workspace (satisfies
`+workspace-p'). If nil or omitted, it defaults to the current workspace."
  (let ((persp (or persp (pm/workspace-current))))
    (unless (pm/workspace-p persp)
      (user-error "Not in a valid workspace (%s)" persp))
    (persp-buffers persp)))

;; Actions

(defun pm/workspace-load (name)
  "Loads a single workspace (named NAME) into the current session. Can only
retrieve perspectives that were explicitly saved with `+workspace-save'.

Returns t if successful, nil otherwise."
  (when (pm/workspace-exists-p name)
    (user-error "A workspace named '%s' already exists." name))
  (persp-load-from-file-by-names
   (expand-file-name pm/workspaces-data-file persp-save-dir)
   *persp-hash* (list name))
  (pm/workspace-exists-p name))

(defun pm/workspace-save (name)
  "Saves a single workspace (NAME) from the current session. Can be loaded again
with `+workspace-load'. NAME can be the string name of a workspace or its
perspective hash table.

Returns t on success, nil otherwise."
  (unless (pm/workspace-exists-p name)
    (error "'%s' is an invalid workspace" name))
  (let ((fname (expand-file-name pm/workspaces-data-file persp-save-dir)))
    (persp-save-to-file-by-names fname *persp-hash* (list name))
    (and (member name (persp-list-persp-names-in-file fname))
         t)))

(defun pm/workspace-new (name)
  "Create a new workspace named NAME. If one already exists, return nil.
Otherwise return t on success, nil otherwise."
  (when (pm/workspace--protected-p name)
    (error "Can't create a new '%s' workspace" name))
  (when (pm/workspace-exists-p name)
    (error "A workspace named '%s' already exists" name))
  (let ((persp (persp-add-new name))
        (+popup--inhibit-transient t))
    (save-window-excursion
      (let ((ignore-window-parameters t)
            (+popup--inhibit-transient t))
        (persp-delete-other-windows))
      (switch-to-buffer (pm/fallback-buffer))
      (setf (persp-window-conf persp)
            (funcall persp-window-state-get-function (selected-frame))))
    persp))

(defun pm/workspace-rename (name new-name)
  "Rename the current workspace named NAME to NEW-NAME. Returns old name on
success, nil otherwise."
  (when (pm/workspace--protected-p name)
    (error "Can't rename '%s' workspace" name))
  (persp-rename new-name (pm/workspace-get name)))

(defun pm/workspace-delete (workspace &optional inhibit-kill-p)
  "Delete the workspace denoted by WORKSPACE, which can be the name of a perspective
or its hash table. If INHIBIT-KILL-P is non-nil, don't kill this workspace's
buffers."
  (unless (stringp workspace)
    (setq workspace (persp-name workspace)))
  (when (pm/workspace--protected-p workspace)
    (error "Can't delete '%s' workspace" workspace))
  (pm/workspace-get workspace) ; error checking
  (persp-kill workspace inhibit-kill-p)
  (not (pm/workspace-exists-p workspace)))

(defun pm/workspace-switch (name &optional auto-create-p)
  "Switch to another workspace named NAME (a string).

If AUTO-CREATE-P is non-nil, create the workspace if it doesn't exist, otherwise
throws an error."
  (unless (pm/workspace-exists-p name)
    (if auto-create-p
        (pm/workspace-new name)
      (error "%s is not an available workspace" name)))
  (let ((old-name (pm/workspace-current-name)))
    (unless (equal old-name name)
      (setq pm/workspace--last
            (or (and (not (string= old-name persp-nil-name))
                     old-name)
                pm/workspaces-main))
      (persp-frame-switch name))
    (equal (pm/workspace-current-name) name)))

;; Display

(defface pm/workspace-tab-selected-face '((t (:inherit highlight)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

(defface pm/workspace-tab-face '((t (:inherit default)))
  "The face for selected tabs displayed by `+workspace/display'"
  :group 'persp-mode)

(defun pm/workspace--tabline (&optional names)
  (let ((names (or names (pm/workspace-list-names)))
        (current-name (pm/workspace-current-name)))
    (mapconcat
     #'identity
     (cl-loop for name in names
              for i to (length names)
              collect
              (propertize (format " [%d] %s " (1+ i) name)
                          'face (if (equal current-name name)
                                    'pm/workspace-tab-selected-face
                                  'pm/workspace-tab-face)))
     " ")))

(defun pm/workspace--message-body (message &optional type)
  (concat (pm/workspace--tabline)
          (propertize " | " 'face 'font-lock-comment-face)
          (propertize (format "%s" message)
                      'face (pcase type
                              ('error 'error)
                              ('warn 'warning)
                              ('success 'success)
                              ('info 'font-lock-comment-face)))))

(defun pm/workspace-message (message &optional type)
  "Show an 'elegant' message in the echo area next to a listing of workspaces."
  (message "%s" (pm/workspace--message-body message type)))

(defun pm/workspace-error (message &optional noerror)
  "Show an 'elegant' error in the echo area next to a listing of workspaces."
  (funcall (if noerror #'message #'error)
           "%s" (pm/workspace--message-body message 'error)))

;;; Hooks

(defun pm/workspaces-ensure-no-nil-workspaces-h (&rest _)
  (when persp-mode
    (dolist (frame (frame-list))
      (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
        ;; Take extra steps to ensure no frame ends up in the nil perspective
        (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                pm/workspaces-main)
                            frame)))))

(defun pm/workspaces-init-first-workspace-h (&rest _)
  "Ensure a main workspace exists."
  (when persp-mode
    (let (persp-before-switch-functions)
      ;; Try our best to hide the nil perspective.
      (when (equal (car persp-names-cache) persp-nil-name)
        (pop persp-names-cache))
      ;; ...and create a *real* main workspace to fill this role.
      (unless (or (persp-get-by-name pm/workspaces-main)
                  ;; Start from 2 b/c persp-mode counts the nil workspace
                  (> (hash-table-count *persp-hash*) 2))
        (persp-add-new pm/workspaces-main))
      ;; HACK Fix #319: the warnings buffer gets swallowed when creating
      ;;      `pm/workspaces-main', so display it ourselves, if it exists.
      (when-let (warnings (get-buffer "*Warnings*"))
        (save-excursion
          (display-buffer-in-side-window
           warnings
           '((window-height . shrink-window-if-larger-than-buffer))))))))

(defun pm/workspaces-init-persp-mode-h ()
  (cond (persp-mode
         ;; `uniquify' breaks persp-mode. It renames old buffers, which causes
         ;; errors when switching between perspective (their buffers are
         ;; serialized by name and persp-mode expects them to have the same
         ;; name when restored).
         (when uniquify-buffer-name-style
           (setq pm/workspace--old-uniquify-style uniquify-buffer-name-style))
         (setq uniquify-buffer-name-style nil)
         ;; Ensure `persp-kill-buffer-query-function' is last
         (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
         (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t)
         ;; Restrict buffer list to workspace
         (advice-add #'buffer-list :override #'pm/workspace-buffer-list))
        (t
         (when pm/workspace--old-uniquify-style
           (setq uniquify-buffer-name-style pm/workspace--old-uniquify-style))
         (advice-remove #'buffer-list #'pm/workspace-buffer-list))))

(defun pm/workspaces-save-winner-data-h (_)
  (when (and (bound-and-true-p winner-mode)
             (get-current-persp))
    (set-persp-parameter
     'winner-ring (list winner-currents
                        winner-ring-alist
                        winner-pending-undo-ring))))

(defun pm/workspaces-load-winner-data-h (_)
  (when (bound-and-true-p winner-mode)
    (cl-destructuring-bind
        (currents alist pending-undo-ring)
        (or (persp-parameter 'winner-ring) (list nil nil nil))
      (setq winner-undo-frame nil
            winner-currents currents
            winner-ring-alist alist
            winner-pending-undo-ring pending-undo-ring))))

(defun pm/workspaces-add-current-buffer-h ()
  "Add current buffer to focused perspective."
  (or (not persp-mode)
      (persp-buffer-filtered-out-p
       (or (buffer-base-buffer (current-buffer))
           (current-buffer))
       persp-add-buffer-on-after-change-major-mode-filter-functions)
      (persp-add-buffer (current-buffer) (get-current-persp) nil nil)))

(defun pm/workspaces-delete-associated-workspace-h (&optional frame)
  "Delete workspace associated with current frame.
A workspace gets associated with a frame when a new frame is interactively
created."
  (when (and persp-mode (not (bound-and-true-p with-editor-mode)))
    (unless frame
      (setq frame (selected-frame)))
    (let ((frame-persp (frame-parameter frame 'workspace)))
      (when (string= frame-persp (pm/workspace-current-name))
        (pm/workspace/delete frame-persp)))))

(defun pm/workspaces-associate-frame-fn (frame &optional _new-frame-p)
  "Create a blank, new perspective and associate it with FRAME."
  (when persp-mode
    (if (not (persp-frame-list-without-daemon))
        (pm/workspace-switch pm/workspaces-main t)
      (with-selected-frame frame
        (pm/workspace-switch (format "#%s" (pm/workspace--generate-id)) t)
        ;; real/unreal buffer functionality
        ;; (unless (doom-real-buffer-p (current-buffer))
        ;;   (switch-to-buffer (pm/fallback-buffer)))
        (set-frame-parameter frame 'workspace (pm/workspace-current-name))
        ;; ensure every buffer has a buffer-predicate
        (persp-set-frame-buffer-predicate frame))
      (run-at-time 0.1 nil #'pm/workspace/display))))

(defun pm/workspaces-reload-indirect-buffers-h (&rest _)
  (dolist (ibc pm/workspaces--indirect-buffers-to-restore)
    (cl-destructuring-bind (buffer-name . base-buffer-name) ibc
      (let ((base-buffer (get-buffer base-buffer-name)))
        (when (buffer-live-p base-buffer)
          (when (get-buffer buffer-name)
            (setq buffer-name (generate-new-buffer-name buffer-name)))
          (make-indirect-buffer base-buffer buffer-name t)))))
  (setq pm/workspaces--indirect-buffers-to-restore nil))

;; configuration

(persp-mode 1)
;; Buffers that are not displayed in any other perspective will be killed when they're removed from a perspective.
(setq persp-autokill-buffer-on-remove 'kill-weak)
;; Prevents persp-mode from resetting window configurations when they're set to nil.
(setq persp-reset-windows-on-nil-window-conf nil)
;; Makes the "nil" perspective (the default one) hidden.
(setq persp-nil-hidden t)
;; Sets the autosave filename to "autosave".
(setq persp-auto-save-fname "autosave")
;; Defines the directory where perspective configurations will be saved. It's set to the "workspaces" directory inside the doom-data-dir.
(setq persp-save-dir (concat user-emacs-directory "workspaces/"))
;; New frames will use the last activated perspective.
(setq persp-set-last-persp-for-new-frames t)
;; When a buffer is added to a perspective, don't automatically switch to it.
(setq persp-switch-to-added-buffer nil)
;; Defines how buffers not belonging to any perspective should be treated when they're closed. Here they will be killed.
(setq persp-kill-foreign-buffer-behaviour 'kill)
;; Configures the behavior when removing buffers from the "nil" perspective.
(setq persp-remove-buffers-from-nil-persp-behaviour nil)
;; Disables the automatic loading of saved perspectives at startup.
(setq persp-auto-resume-time -1)
;; Sets the autosave option based on whether Emacs is running interactively. If in noninteractive mode, it's set to 0 (disabled), otherwise, it's set to 1 (enabled), meaning perspectives will be auto-saved when Emacs is killed.
(setq persp-auto-save-opt (if noninteractive 0 1))

(add-hook 'persp-mode-hook #'pm/workspaces-ensure-no-nil-workspaces-h)
(add-hook 'persp-after-load-state-functions #'pm/workspaces-ensure-no-nil-workspaces-h)

(add-hook 'persp-mode-hook #'pm/workspaces-init-first-workspace-h)
(add-hook 'persp-mode-hook #'pm/workspaces-init-persp-mode-h)

(add-to-list 'window-persistent-parameters '(winner-ring . t))

(add-hook 'persp-before-deactivate-functions #'pm/workspaces-save-winner-data-h)
(add-hook 'persp-activated-functions #'pm/workspaces-load-winner-data-h)

;; Needs something emulating global buffer switch hook
;; (add-hook 'doom-switch-buffer-hook #'pm/workspaces-add-current-buffer-h)

;; Doom utilises a categorisation framework for buffers to filter them into real - buffers which are useful, and unreal - buffers which are of little import, and can be cast aside readily
;; (add-hook 'persp-add-buffer-on-after-change-major-mode-filter-functions
;;           #'doom-unreal-buffer-p)

;; 
;; (defadvice! +workspaces--evil-alternate-buffer-a (&optional window)
;;             "Make `evil-alternate-buffer' ignore buffers outside the current workspace."
;;             :override #'evil-alternate-buffer
;;             (let* ((prev-buffers
;;                     (if persp-mode
;;                         (cl-remove-if-not #'persp-contain-buffer-p (window-prev-buffers)
;;                                           :key #'car)
;;                       (window-prev-buffers)))
;;                    (head (car prev-buffers)))
;;               (if (eq (car head) (window-buffer window))
;;                   (cadr prev-buffers)
;;                 head)))

;;
;; (defadvice! +workspaces-remove-dead-buffers-a (persp)
;;             :before #'persp-buffers-to-savelist
;;             (when (perspective-p persp)
;;               ;; HACK Can't use `persp-buffers' because of a race condition with its gv
;;               ;;      getter/setter not being defined in time.
;;               (setf (aref persp 2)
;;                     (cl-delete-if-not #'persp-get-buffer-or-null (persp-buffers persp)))))

;;
;; (define-key! persp-mode-map
;;              [remap delete-window] #'+workspace/close-window-or-workspace
;;              [remap evil-window-delete] #'+workspace/close-window-or-workspace)


(setq persp-init-frame-behaviour t
      persp-init-new-frame-behaviour-override nil
      persp-interactive-init-frame-behaviour-override #'pm/workspaces-associate-frame-fn
      persp-emacsclient-init-frame-behaviour-override #'pm/workspaces-associate-frame-fn)

(add-hook 'delete-frame-functions #'pm/workspaces-delete-associated-workspace-h)
(add-hook 'server-done-hook #'pm/workspaces-delete-associated-workspace-h)

;; Move to project
;; (setq projectile-switch-project-action (lambda () (+workspaces-set-project-action-fn) (+workspaces-switch-to-project-h))
;;         counsel-projectile-switch-project-action
;;         '(1 ("o" +workspaces-switch-to-project-h "open project in new workspace")
;;             ("O" counsel-projectile-switch-project-action "jump to a project buffer or file")
;;             ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
;;             ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
;;             ("D" counsel-projectile-switch-project-action-dired "open project in dired")
;;             ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
;;             ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
;;             ("w" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
;;             ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
;;             ("r" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
;;             ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
;;             ("C" counsel-projectile-switch-project-action-configure "run project configure command")
;;             ("e" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
;;             ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
;;             ("s" (lambda (project)
;;                    (let ((projectile-switch-project-action
;;                           (lambda () (call-interactively #'+ivy/project-search))))
;;                      (counsel-projectile-switch-project-by-name project))) "search project")
;;             ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
;;             ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
;;             ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
;;             ("X" counsel-projectile-switch-project-action-org-capture "org-capture into project")))

;; Move to completion
;; (when (modulep! :completion ivy)
;;   (after! ivy-rich
;;           (cl-callf plist-put ivy-rich-display-transformers-list
;;             '+workspace/switch-to
;;             '(:columns ((ivy-rich-candidate (:width 50))
;;                         (+workspace--ivy-rich-preview))))))

;; (when (modulep! :completion helm)
;;   (after! helm-projectile
;;           (setcar helm-source-projectile-projects-actions
;;                   '("Switch to Project" . +workspaces-switch-to-project-h))))

;; Needs buffer categorisation, only save real buffer on close
;; (advice-add #'persp-asave-on-exit :around #'+workspaces-autosave-real-buffers-a)

(add-hook 'persp-before-deactivate-functions #'deactivate-mark)

;; As of writing not using posframe
;; (after! posframe
;;         (add-hook! 'persp-after-load-state-functions
;;                    (defun +workspaces-delete-all-posframes-h (&rest _)
;;                      (posframe-delete-all))))

;; Buffers to filter out on perspective saving
;; (add-hook! 'persp-filter-save-buffers-functions
;;            (defun +workspaces-dead-buffer-p (buf)
;;              ;; Fix #1525: Ignore dead buffers in PERSP's buffer list
;;              (not (buffer-live-p buf)))
;;            (defun +workspaces-remote-buffer-p (buf)
;;              ;; And don't save TRAMP buffers; they're super slow to restore
;;              (let ((dir (buffer-local-value 'default-directory buf)))
;;                (ignore-errors (file-remote-p dir)))))

;; Usage with bookmarks
(add-hook 'bookmark-after-jump-hook #'pm/workspaces-add-current-buffer-h)

;;; eshell
(persp-def-buffer-save/load
 :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
 :save-vars '(major-mode default-directory))

;; compile
(persp-def-buffer-save/load
 :mode 'compilation-mode :tag-symbol 'def-compilation-buffer
 :save-vars '(major-mode default-directory compilation-directory
                         compilation-environment compilation-arguments))

;; magit
(persp-def-buffer-save/load
 :mode 'magit-status-mode :tag-symbol 'def-magit-status-buffer
 :save-vars '(default-directory)
 :load-function (lambda (savelist &rest _)
                  (cl-destructuring-bind (buffer-name vars &rest _rest) (cdr savelist)
                    (magit-status (alist-get 'default-directory vars)))))

;; Restore indirect buffers
(persp-def-buffer-save/load
 :tag-symbol 'def-indirect-buffer
 :predicate #'buffer-base-buffer
 :save-function (lambda (buf tag vars)
                  (list tag (buffer-name buf) vars
                        (buffer-name (buffer-base-buffer buf))))
 :load-function (lambda (savelist &rest _rest)
                  (cl-destructuring-bind (buf-name _vars base-buf-name &rest _)
                      (cdr savelist)
                    (push (cons buf-name base-buf-name)
                          pm/workspaces--indirect-buffers-to-restore)
                    nil)))

(add-hook 'persp-after-load-state-functions #'pm/workspaces-reload-indirect-buffers-h)

;; Not necessary unless tab bar mode working
;; (add-hook! 'tab-bar-mode-hook
;;            (defun +workspaces-set-up-tab-bar-integration-h ()
;;              (add-hook 'persp-before-deactivate-functions #'+workspaces-save-tab-bar-data-h)
;;              (add-hook 'persp-activated-functions #'+workspaces-load-tab-bar-data-h)
;;              ;; Load and save configurations for tab-bar.
;;              (add-hook 'persp-before-save-state-to-file-functions #'+workspaces-save-tab-bar-data-to-file-h)
;;              (+workspaces-load-tab-bar-data-from-file-h)))

;; Commands

;; Needs session management
;; (defalias 'pm/workspace/restore-last-session #'doom/quickload-session)

(defun pm/workspace/display ()
  "Display a list of workspaces (like tabs) in the echo area."
  (interactive)
  (let (message-log-max)
    (message "%s" (pm/workspace--tabline))))

(defun pm/workspace/load (name)
  "Load a workspace and switch to it. If called with C-u, try to reload the
current workspace (by name) from session files."
  (interactive
   (list
    (if current-prefix-arg
        (pm/workspace-current-name)
      (completing-read
       "Workspace to load: "
       (persp-list-persp-names-in-file
        (expand-file-name pm/workspaces-data-file persp-save-dir))))))
  (if (not (pm/workspace-load name))
      (pm/workspace-error (format "Couldn't load workspace %s" name))
    (pm/workspace/switch-to name)
    (pm/workspace/display)))

(defun pm/workspace/save (name)
  "Save the current workspace. If called with C-u, autosave the current
workspace."
  (interactive
   (list
    (if current-prefix-arg
        (pm/workspace-current-name)
      (completing-read "Workspace to save: " (pm/workspace-list-names)))))
  (if (pm/workspace-save name)
      (pm/workspace-message (format "'%s' workspace saved" name) 'success)
    (pm/workspace-error (format "Couldn't save workspace %s" name))))

(defun pm/workspace/rename (new-name)
  "Rename the current workspace."
  (interactive (list (completing-read "New workspace name: " (list (pm/workspace-current-name)))))
  (condition-case-unless-debug ex
      (let* ((current-name (pm/workspace-current-name))
             (old-name (pm/workspace-rename current-name new-name)))
        (unless old-name
          (error "Failed to rename %s" current-name))
        (pm/workspace-message (format "Renamed '%s'->'%s'" old-name new-name) 'success))
    ('error (pm/workspace-error ex t))))

(defun pm/workspace/delete (name)
  "Delete this workspace. If called with C-u, prompts you for the name of the
workspace to delete."
  (interactive
   (let ((current-name (pm/workspace-current-name)))
     (list
      (if current-prefix-arg
          (completing-read (format "Delete workspace (default: %s): " current-name)
                           (pm/workspace-list-names)
                           nil nil nil nil current-name)
        current-name))))
  (condition-case-unless-debug ex
      ;; REVIEW refactor me
      (let ((workspaces (pm/workspace-list-names)))
        (if (not (member name workspaces))
            (pm/workspace-message (format "'%s' workspace doesn't exist" name) 'warn)
          (cond ((delq (selected-frame) (persp-frames-with-persp (get-frame-persp)))
                 (user-error "Can't close workspace, it's visible in another frame"))
                ((not (equal (pm/workspace-current-name) name))
                 (pm/workspace-delete name))
                ((cdr workspaces)
                 (pm/workspace-delete name)
                 (pm/workspace-switch
                  (if (pm/workspace-exists-p pm/workspace--last)
                      pm/workspace--last
                    (car (pm/workspace-list-names))))
                 ;; real vs unreal buffer logic
                 ;; (unless (doom-buffer-frame-predicate (window-buffer))
                 ;;   (switch-to-buffer (doom-fallback-buffer)))
                 )
                (t
                 (pm/workspace-switch pm/workspaces-main t)
                 (unless (string= (car workspaces) pm/workspaces-main)
                   (pm/workspace-delete name))
                 (pm/kill-all-buffers (buffer-list))))
          (pm/workspace-message (format "Deleted '%s' workspace" name) 'success)))
    ('error (pm/workspace-error ex t))))

(defun pm/workspace/kill-session (&optional interactive)
  "Delete the current session, all workspaces, windows and their buffers."
  (interactive (list t))
  (let ((windows (length (window-list)))
        (persps (length (pm/workspace-list-names)))
        (buffers 0))
    (let ((persp-autokill-buffer-on-remove t))
      (unless (cl-every #'pm/workspace-delete (pm/workspace-list-names))
        (pm/workspace-error "Could not clear session")))
    (pm/workspace-switch pm/workspaces-main t)
    (setq buffers (pm/kill-all-buffers (buffer-list)))
    (when interactive
      (message "Killed %d workspace(s), %d window(s) & %d buffer(s)"
               persps windows buffers))))

(defun pm/workspace/kill-session-and-quit ()
  "Kill emacs without saving anything."
  (interactive)
  (let ((persp-auto-save-opt 0))
    (kill-emacs)))

(defun pm/workspace/new (&optional name clone-p)
  "Create a new workspace named NAME. If CLONE-P is non-nil, clone the current
workspace, otherwise the new workspace is blank."
  (interactive (list nil current-prefix-arg))
  (unless name
    (setq name (format "#%s" (pm/workspace--generate-id))))
  (condition-case e
      (cond ((pm/workspace-exists-p name)
             (error "%s already exists" name))
            (clone-p (persp-copy name t))
            (t
             (pm/workspace-switch name t)
             (pm/workspace/display)))
    ((debug error) (pm/workspace-error (cadr e) t))))

(defun pm/workspace/new-named (name)
  "Create a new workspace with a given NAME."
  (interactive "sWorkspace Name: ")
  (pm/workspace/new name))

(defun pm/workspace/switch-to (index)
  "Switch to a workspace at a given INDEX. A negative number will start from the
end of the workspace list."
  (interactive
   (list (or current-prefix-arg
             (completing-read "Switch to workspace: " (+workspace-list-names)))))
  (when (and (stringp index)
             (string-match-p "^[0-9]+$" index))
    (setq index (string-to-number index)))
  (condition-case-unless-debug ex
      (let ((names (pm/workspace-list-names))
            (old-name (pm/workspace-current-name)))
        (cond ((numberp index)
               (let ((dest (nth index names)))
                 (unless dest
                   (error "No workspace at #%s" (1+ index)))
                 (pm/workspace-switch dest)))
              ((stringp index)
               (pm/workspace-switch index t))
              (t
               (error "Not a valid index: %s" index)))
        (unless (called-interactively-p 'interactive)
          (if (equal (pm/workspace-current-name) old-name)
              (pm/workspace-message (format "Already in %s" old-name) 'warn)
            (pm/workspace/display))))
    ('error (pm/workspace-error (cadr ex) t))))

(dotimes (i 9)
  (defalias (intern (format "pm/workspace/switch-to-%d" i))
    (lambda () (interactive) (pm/workspace/switch-to i))
    (format "Switch to workspace #%d" (1+ i))))

(defun pm/workspace/switch-to-final ()
  "Switch to the final workspace in open workspaces."
  (interactive)
  (pm/workspace/switch-to (car (last (pm/workspace-list-names)))))

(defun pm/workspace/other ()
  "Switch to the last activated workspace."
  (interactive)
  (pm/workspace/switch-to pm/workspace--last))

(defun pm/workspace/cycle (n)
  "Cycle n workspaces to the right (default) or left."
  (interactive (list 1))
  (let ((current-name (pm/workspace-current-name)))
    (if (equal current-name persp-nil-name)
        (pm/workspace-switch pm/workspaces-main t)
      (condition-case-unless-debug ex
          (let* ((persps (pm/workspace-list-names))
                 (perspc (length persps))
                 (index (cl-position current-name persps)))
            (when (= perspc 1)
              (user-error "No other workspaces"))
            (pm/workspace/switch-to (% (+ index n perspc) perspc))
            (unless (called-interactively-p 'interactive)
              (pm/workspace/display)))
        ('user-error (pm/workspace-error (cadr ex) t))
        ('error (pm/workspace-error ex t))))))

(defun pm/workspace/switch-left ()  (interactive) (pm/workspace/cycle -1))
(defun pm/workspace/switch-right () (interactive) (pm/workspace/cycle +1))

(defun pm/workspace/close-window-or-workspace ()
  "Close the selected window. If it's the last window in the workspace, either
close the workspace (as well as its associated frame, if one exists) and move to
the next."
  (interactive)
  (let ((delete-window-fn (if (featurep 'evil) #'evil-window-delete #'delete-window)))
    (if (window-dedicated-p)
        (funcall delete-window-fn)
      (let ((current-persp-name (pm/workspace-current-name)))
        (cond ((or (pm/workspace--protected-p current-persp-name)
                   (cdr (pm/visible-windows)))
               (funcall delete-window-fn))

              ((cdr (pm/workspace-list-names))
               (let ((frame-persp (frame-parameter nil 'workspace)))
                 (if (string= frame-persp (pm/workspace-current-name))
                     (delete-frame)
                   (pm/workspace/delete current-persp-name))))

              ((pm/workspace-error "Can't delete last workspace" t)))))))

(defun pm/workspace/swap-left (&optional count)
  "Swap the current workspace with the COUNTth workspace on its left."
  (interactive "p")
  (let* ((current-name (pm/workspace-current-name))
         (count (or count 1))
         (index (- (cl-position current-name persp-names-cache :test #'equal)
                   count))
         (names (remove current-name persp-names-cache)))
    (unless names
      (user-error "Only one workspace"))
    (let ((index (min (max 0 index) (length names))))
      (setq persp-names-cache
            (append (cl-subseq names 0 index)
                    (list current-name)
                    (cl-subseq names index))))
    (when (called-interactively-p 'any)
      (pm/workspace/display))))

(defun pm/workspace/swap-right (&optional count)
  "Swap the current workspace with the COUNTth workspace on its right."
  (interactive "p")
  (funcall-interactively #'pm/workspace/swap-left (- count)))

(provide 'workspaces.el)
