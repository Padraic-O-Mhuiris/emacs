;;; projects.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:
(cl-defun pm/project-note-read ()
  (interactive)
  (pm/note-read
   :prompt "Select from projects: "
   :filter-fn (lambda (node)
                (cl-some (lambda (tag)
                           (string= tag "project"))
                         (org-roam-node-tags node)))))

(defun pm/project-capture-templates ()
  `(("a" "Insert project todo" entry
     ,pm/note-todo-entry
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Tasks"))
     :prepend t)
    ("b" "Insert project idea" entry
     ,pm/note-idea-entry
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Ideas"))
     :prepend t)
    ("c" "Insert project link" item
     "%(org-cliplink-capture)"
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Links"))
     :prepend t)
    ("d" "Insert project journal" entry
     ,(pm/template-entry-builder :title-content (concat "[ " (pm/current-time) " ]\n%?")
                                 :no-properties t
                                 :levels 3)
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,`("Journal" ,(concat "[ " (pm/todays-date) " ]")))
     :prepend t)))

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
  (let ((node (pm/project-note-read)))
    (if (org-roam-node-file node)
        (pm/project-capture-existing node)
      (pm/project-capture-new node))))

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

(cl-defun pm/project-note-capture--project-note (&optional capture-key)
  (org-roam-capture-
   :goto nil
   :info nil
   :keys capture-key
   :node (org-roam-node-at-point)
   :templates (pm/project-capture-templates)
   :props '(:immediate-finish t :jump-to-captured t)))

;; Keep capture template structure uniform
(cl-defun pm/project-note-capture-2 (&optional capture-key)
  (let ((ctx (pm/project-note-context)))
    (cond
     ((eq ctx 'project-note)
      (pm/project-note-capture--project-note capture-key))
     ((eq ctx 'project)
      ;; Find associated project
      ;; - If none exists, create a new one with suggested title or select from list
      nil
      )
     (t
      ;; Otherwise, select from projects and capture
      nil
      ))))

(cl-defun pm/project-note-capture-todo ()
  (interactive)
  (pm/project-note-capture-2 "a"))
  

(cl-defun pm/project-note-capture-idea ()
  (interactive)
  (pm/project-note-capture-2 "b"))

(cl-defun pm/project-note-capture-link ()
  (interactive)
  (pm/project-note-capture-2 "c"))

(cl-defun pm/project-note-capture-journal ()
  (interactive)
  (pm/project-note-capture-2 "d"))

(pm/leader
  "np" '(nil :which-key "capture project")
  "npf" '(pm/project-note-read :which-key "find project note")
  "npt" '(pm/project-note-capture-todo :which-key "capture project todo")
  "npi" '(pm/project-note-capture-idea :which-key "capture project idea")
  "npl" '(pm/project-note-capture-idea :which-key "capture project link")
  "npj" '(pm/project-note-capture-idea :which-key "capture project journal"))

(provide 'projects.el)
