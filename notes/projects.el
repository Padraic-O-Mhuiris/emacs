;;; projects.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:
(cl-defun pm/project-read ()
  (interactive)
  (pm/note-read
   :prompt "Select from projects: "
   :filter-fn (lambda (node)
                (cl-some (lambda (tag)
                           (string= tag "project"))
                         (org-roam-node-tags node)))))

(defun pm/project-capture-templates ()
  `(("a" "Edit project" plain
     ,pm/note-basic-entry
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Abstract")))
    ("b" "Insert project todo" entry
     ,pm/note-todo-entry
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Tasks")))
    ("c" "Insert project idea" entry
     ,pm/note-idea-entry
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Ideas")))
    ("d" "Insert project link" item
     "%(org-cliplink-capture)"
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Links")))
    ("e" "Insert project journal" entry
     ,(pm/template-entry-builder :title-content (concat "[ " (pm/current-time) " ]\n%?")
                                 :no-properties t
                                 :levels 3)
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,`("Journal" ,(concat "[ " (pm/todays-date) " ]"))))))

(cl-defun pm/project-capture-existing (node)
  (org-roam-capture-
   :goto nil
   :info nil
   :keys nil
   :node node
   :templates (project-capture-templates)
   :props '(:unnarrowed t :prepend t)))

(cl-defun pm/project-capture-new (node)
  (org-roam-capture-
   :goto nil
   :info nil
   :keys nil
   :templates `(("a" "Basic project note" plain
                 ,pm/note-basic-entry
                 :target (file+head+olp
                          ,pm/project-note-name-template
                          ,(pm/template-head-builder
                            :tags '("project")
                            :prompt-for-tags t
                            :headings '("Abstract" "Tasks" "Links" "Journal" "Ideas"))
                          ,'("Abstract"))))
   :node node
   :props '(:unnarrowed t :empty-lines 1)))

(cl-defun pm/project-capture ()
  (interactive)
  (if (file-directory-p (concat org-directory "/project"))
      nil
    (progn
      (make-directory (concat org-directory "/project"))))
  (let ((node (pm/project-read)))
    (if (org-roam-node-file node)
        (pm/project-capture-existing node)
      (pm/project-capture-new node))))

(cl-defun pm/project-note-capture--context ()
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

(pm/leader
  "t" '(pm/project-note-capture--context :which-key "test-placeholder"))


(pm/leader
  "np" '(pm/project-capture :which-key "capture project"))

(provide 'projects.el)
