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
  (car (pm/collect-org-keywords-from-file
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

(defun pm/project-capture-new-template ()
  `(("a" "Basic project note" plain
     ,pm/note-basic-entry
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :prompt-for-tags t
                :headings '("Abstract" "Tasks" "Links" "Journal" "Ideas"))
              ,'("Abstract")))))

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

(cl-defun pm/project-note--find-node-by-project-path (project-path)
  (mapcar
   #'(lambda (x) (org-roam-populate (org-roam-node-create :id (car (cdr x)))))
   (cl-remove-if-not
    #'(lambda (file)
        (string=
         project-path
         (car (cdr (car (pm/collect-org-keywords-from-file (car file) '("project_path")))))))
    (org-roam-db-query
     [:select [nodes:file nodes:id]
              :from nodes
              :join tags
              :on (= nodes:id tags:node-id)
              :where (like tags:tag "%project%")]))))

(cl-defun pm/project-note-capture--project (&optional capture-key)
  (let* ((project-path (projectile-project-p))
         (nodes (pm/project-note--find-node-by-project-path project-path))
         (node (car nodes)))
    (if (> (length nodes) 1)
        (user-error "Found two project notes for path: %s" project-path)
      (if node
          (org-roam-capture-
           :goto nil
           :info nil
           :keys capture-key
           :node node
           :templates (pm/project-capture-templates)
           :props '(:unnarrowed t :kill-buffer t))
        (let* ((project-node (pm/note-read
                              :prompt "Select/Create project: "
                              :filter-fn
                              (lambda (node)
                                (and (pm/note-has-project-tag node)
                                     (not (pm/note-has-project-path node)))))))
          (if (org-roam-node-file project-node)
              (progn
                (with-temp-buffer
                  (insert-file-contents (org-roam-node-file project-node))
                  (org-mode)
                  (org-roam-set-keyword "project_path" project-path)
                  (write-file (org-roam-node-file project-node)))
                (org-roam-capture-
                 :goto nil
                 :info nil
                 :keys capture-key
                 :node project-node
                 :templates (pm/project-capture-templates)
                 :props '(:unnarrowed t :kill-buffer t)))
            (org-roam-capture-
             :goto nil
             :info nil
             :keys nil
             :node project-node
             :templates (pm/project-capture-new-template)
             :props '(:unnarrowed t))))))))

(cl-defun pm/project-note-capture--other (&optional capture-key)
  (let ((node (pm/note-read
               :prompt "Select/Create project: "
               :filter-fn #'pm/note-has-project-tag)))
    (if (org-roam-node-file node)
        (org-roam-capture-
         :goto nil
         :info nil
         :keys capture-key
         :node node
         :templates (pm/project-capture-templates)
         :props '(:unnarrowed t :kill-buffer t))
      (org-roam-capture-
       :goto nil
       :info nil
       :keys nil
       :node project-node
       :templates (pm/project-capture-new-template)
       :props '(:unnarrowed t :kill-buffer t)))))

(cl-defun pm/project-note-capture (&optional capture-key)
  (let ((ctx (pm/project-note-context)))
    (cond
     ((eq ctx 'project-note)
      (pm/project-note-capture--project-note capture-key))
     ((eq ctx 'project)
      (pm/project-note-capture--project capture-key))
     (t (pm/project-note-capture--other capture-key)))))

(cl-defun pm/project-note-capture-todo ()
  (interactive)
  (pm/project-note-capture "a"))
  
(cl-defun pm/project-note-capture-idea ()
  (interactive)
  (pm/project-note-capture "b"))

(cl-defun pm/project-note-capture-link ()
  (interactive)
  (pm/project-note-capture "c"))

(cl-defun pm/project-note-capture-journal ()
  (interactive)
  (pm/project-note-capture "d"))

(pm/leader
  "np" '(nil :which-key "capture project")
  "npf" '(pm/project-note-read :which-key "find project note")
  "npt" '(pm/project-note-capture-todo :which-key "capture project todo")
  "npi" '(pm/project-note-capture-idea :which-key "capture project idea")
  "npl" '(pm/project-note-capture-link :which-key "capture project link")
  "npj" '(pm/project-note-capture-journal :which-key "capture project journal"))

(provide 'projects.el)
