;;; projects.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:
(cl-defun pm/note-has-project-tag (node)
  (cl-some
   (lambda (tag)
     (string= tag "project"))
   (org-roam-node-tags node)))

(cl-defun pm/note-has-project-path (node)
  (car (pm/get-org-keywords-from-file
        (org-roam-node-file node)
        '("project_path"))))

(cl-defun pm/project-note-read ()
  (interactive)
  (pm/note-read
   :prompt "Select from projects: "
   :filter-fn (lambda (node)
                (cl-some (lambda (tag)
                           (string= tag "project"))
                         (org-roam-node-tags node)))))

(defun pm/project-note-capture-template-name ()
  ""
  "project/${slug}.org")

(defun pm/project-note-capture-template-head ()
  (pm/template-head-builder
   :tags '("project")
   :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal")))

(defun pm/project-note-capture-template-target (olp)
  `(file+head+olp
    ,(pm/project-note-capture-template-name)
    ,(pm/project-note-capture-template-head)
    ,olp))

(defun pm/project-note-capture-todo-template ()
  `("a" "Insert project todo" entry
    ,pm/note-todo-entry
    :target ,(pm/project-note-capture-template-target '("Tasks"))))

(defun pm/project-note-capture-idea-template ()
  `("b" "Insert project idea" entry
    ,pm/note-idea-entry
    :target ,(pm/project-note-capture-template-target '("Ideas"))))

(defun pm/project-note-capture-link-template ()
  `("c" "Insert project link" item
    ,pm/note-link-entry
    :target ,(pm/project-note-capture-template-target '("Links"))))

(defun pm/project-note-capture-journal-template ()
  `("d" "Insert project journal" entry
    ,(pm/template-entry-builder
      :title-content (concat "[ " (pm/current-time) " ]")
      :entry-content "%?"
      :no-properties t
      :levels 3)
    :target ,(pm/project-note-capture-template-target
              `("Journal" ,(concat "[ " (pm/todays-date) " ]")))
    :prepend t))

(defun pm/project-note-capture-goto-template ()
  `("e" "Goto project" plain
    "%?"
    :target ,(pm/project-note-capture-template-target '("Abstract"))))

(defun pm/project-note-capture-existing-templates ()
  `(,(pm/project-note-capture-todo-template)
    ,(pm/project-note-capture-idea-template)
    ,(pm/project-note-capture-link-template)
    ,(pm/project-note-capture-journal-template)
    ,(pm/project-note-capture-goto-template)))

(cl-defun pm/project-note-capture (&key (key nil)
                                        (node nil)
                                        (templates (pm/project-note-capture-existing-templates))
                                        (props '()))
  (org-roam-capture-
   :goto nil
   :info nil
   :keys key
   :node node
   :templates templates
   :props (append props '(:prepend t))))

(cl-defun pm/project-note-capture-new (&key node project-path)
  (let* ((target
          `(file+head+olp
            ,(pm/project-note-capture-template-name)
            ,(pm/template-head-builder
              :tags '("project")
              :project-path project-path
              :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
            ,'("Abstract")))
         (templates
          `(("a" "Project Note" plain
             "%?"
             :target ,target
             :unnarrowed t))))
    (pm/project-note-capture
     :node node
     :templates templates)))

(cl-defun pm/project-note-context ()
  "Determines the context a project note capture process exists:
Returns 4 cases:

'project-note - when in a buffer of one of the specific project notes
'note - when in a non-project note
'project - when in a projectile recognised project with path
nil - when in any non-project non-note buffer"
  (cond
   ((and (fboundp 'org-roam-buffer-p)
         (org-roam-buffer-p))
    (if (cl-some (lambda (tag)
                   (string= tag "project"))
                 (org-roam-node-tags (org-roam-node-at-point)))
        'project-note
      'note))
   ((and (fboundp 'projectile-project-p)
         (projectile-project-p))
    'project)
   (t nil)))

(cl-defun pm/project-note--find-node-by-project-path (project-path)
  (mapcar
   #'(lambda (x) (org-roam-populate (org-roam-node-create :id (car (cdr x)))))
   (cl-remove-if-not
    #'(lambda (file)
        (string=
         project-path
         (car (cdr (car (pm/get-org-keywords-from-file (car file) '("project_path")))))))
    (org-roam-db-query
     [:select [nodes:file nodes:id]
              :from nodes
              :join tags
              :on (= nodes:id tags:node-id)
              :where (like tags:tag "%project%")]))))

(cl-defun pm/project-note-read--without-project-path-keyword (initial-input)
  (pm/note-read
   :initial-input initial-input
   :prompt "Select/Create project: "
   :filter-fn (lambda (node)
                (and (pm/note-has-project-tag node)
                     (not (pm/note-has-project-path node))))))

(cl-defun pm/project-note-capture--project-note (&optional capture-key)
  (pm/project-note-capture
   :key capture-key
   :node (org-roam-node-at-point)
   :props '(:immediate-finish t :jump-to-captured t)))

(cl-defun pm/project-note-capture--project (&optional capture-key)
  (let* ((project-path (projectile-project-p))
         (nodes (pm/project-note--find-node-by-project-path project-path))
         (node (car nodes)))
    (if (> (length nodes) 1)
        (user-error "Found two project notes for path: %s" project-path)
      (if node
          (pm/project-note-capture
           :key capture-key
           :node node
           :props (if (string= capture-key "e")
                      '(:immediate-finish t :jump-to-captured t)
                    '(:unnarrowed t :kill-buffer t)))
        (let ((project-node (org-roam-populate
                             (pm/project-note-read--without-project-path-keyword
                              (projectile-project-name)))))
          (if (org-roam-node-file project-node)
              (progn
                (pm/note-insert-keyword-and-value
                 :node project-node
                 :key "project_path"
                 :value project-path)
                (pm/project-note-capture
                 :key capture-key
                 :node project-node
                 :props '(:unnarrowed t :kill-buffer t)))
            (pm/project-note-capture-new
             :node project-node
             :project-path project-path)))))))

(cl-defun pm/project-note-capture--other (&optional capture-key)
  (let ((node (pm/project-note-read)))
    (if (org-roam-node-file node)
        (pm/project-note-capture
         :key capture-key
         :node node
         :props '(:unnarrowed t :kill-buffer t)))
    (pm/project-note-capture-new
     :node node
     :project-path nil)))

(cl-defun pm/project-note (&optional capture-key)
  (let ((ctx (pm/project-note-context)))
    (cond
     ((eq ctx 'project-note)
      (pm/project-note-capture--project-note capture-key))
     ((eq ctx 'project)
      (pm/project-note-capture--project capture-key))
     (t (pm/project-note-capture--other capture-key)))))

(cl-defun pm/project-note-todo ()
  (interactive)
  (pm/project-note "a"))
  
(cl-defun pm/project-note-idea ()
  (interactive)
  (pm/project-note "b"))

(cl-defun pm/project-note-link ()
  (interactive)
  (pm/project-note "c"))

(cl-defun pm/project-note-journal ()
  (interactive)
  (pm/project-note "d"))

(cl-defun pm/project-note-goto ()
  (interactive)
  (pm/project-note "e"))

(pm/leader
  "np" '(nil :which-key "capture project")
  "npf" '(pm/project-note-read :which-key "find project note")
  "npg" '(pm/project-note-goto :which-key "goto project note")
  "npt" '(pm/project-note-todo :which-key "capture project todo")
  "npi" '(pm/project-note-idea :which-key "capture project idea")
  "npl" '(pm/project-note-link :which-key "capture project link")
  "npj" '(pm/project-note-journal :which-key "capture project journal"))

(provide 'projects.el)
