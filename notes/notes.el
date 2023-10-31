;;; notes.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:
(setopt org-roam-directory org-directory
        org-roam-dailies-directory "daily/")
(setq org-roam-database-connector 'sqlite-builtin)

(org-roam-db-autosync-mode)

(pm/leader
  "n" '(:ignore t :which-key "notes"))

(load-file (concat user-emacs-directory "notes/projects.el"))

;; y/n prompt prevention in org-roam captures
(defun pm/return-t (orig-fun &rest args) t)
(defun pm/disable-yornp (orig-fun &rest args)
  (advice-add 'yes-or-no-p :around #'pm/return-t)
  (advice-add 'y-or-n-p :around #'pm/return-t)
  (let ((res (apply orig-fun args)))
    (advice-remove 'yes-or-no-p #'pm/return-t)
    (advice-remove 'y-or-n-p #'pm/return-t)
    res))
(advice-add 'org-roam-capture--finalize :around #'pm/disable-yornp)

;;; Node display
(defun pm/rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(cl-defmethod org-roam-node-pm/filetitle ((node org-roam-node))
  "Return the value of \"#+title:\" (if any) from file that NODE resides in.
If there's no file-level title in the file, return empty string."
  (or (if (= (org-roam-node-level node) 0)
          (org-roam-node-title node)
        (org-roam-node-file-title node))
      ""))

(cl-defmethod org-roam-node-pm/hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, they will be stripped out."
  (let ((title     (org-roam-node-title node))
        (olp       (org-roam-node-olp   node))
        (level     (org-roam-node-level node))
        (filetitle (org-roam-node-pm/filetitle node))
        (separator (propertize "<>" 'face 'shadow)))
    (cl-case level
      ;; node is a top-level file
      (0 filetitle)
      ;; node is a level 1 heading
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      ;; node is a heading with an arbitrary outline path
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp separator) 'face '(shadow italic))
                 separator title)))))

(cl-defmethod org-roam-node-pm/type ((node org-roam-node))
  "Return the directory relative to `org-roam-directory' as a note's \"type\"."
  (when-let (dir (thread-first
                   node
                   (org-roam-node-file)
                   (file-relative-name org-roam-directory)
                   (file-name-directory)))
    (directory-file-name dir)))

(cl-defmethod org-roam-node-pm/tags ((node org-roam-node))
  "Return tags formatted in the same way how they appear in org files."
  (cl-remove-if (pm/rpartial
                 #'member (delq
                           nil (append
                                (list (bound-and-true-p org-archive-tag)
                                      (bound-and-true-p org-attach-auto-tag))
                                (bound-and-true-p org-num-skip-tags))))
                (org-roam-node-tags node)))

(setq org-roam-node-display-template
      (format "${pm/hierarchy:*} %s %s"
              (propertize "${pm/type}" 'face 'font-lock-keyword-face)
              (propertize "${pm/tags:*}" 'face '(:inherit org-tag :box nil))))

(add-to-list 'org-roam-node-template-prefixes '("pm/tags" . "#"))
(add-to-list 'org-roam-node-template-prefixes '("pm/type" . "@"))

;; Note Entries -- probably should delete in favour of more custom solutions
(defvar pm/note-basic-entry (pm/template-entry-builder :entry-content "%?" :no-properties t))
(defvar pm/note-todo-entry (pm/template-entry-builder :todo-state "TODO" :levels 2 :title-content "%?"))
(defvar pm/note-journal-entry (pm/template-entry-builder :title-content "[%<%T>]\n %?" :levels 2 :no-properties t))
(defvar pm/note-idea-entry (pm/template-entry-builder :todo-state "IDEA" :levels 2 :title-content "%?"))
(defvar pm/note-link-entry (pm/template-entry-builder
                            :no-properties t
                            :title-content "%(org-cliplink-capture)"))

;; Note names
(defvar pm/default-note-name-template "%<%s>__${slug}.org")
(defvar pm/project-note-name-template "project/${slug}.org")
(defvar pm/daily-note-name-template "%<%Y-%m-%d>.org")
(defvar pm/people-note-name-template "people/<%s>__${slug}.org")

;; Note targets - are these used?
(defvar pm/basic-note-target `(file+head ,pm/default-note-name-template ,(pm/template-head-builder)))
(defvar pm/action-note-target
  `(file+head
    ,pm/default-note-name-template
    ,(pm/template-head-builder
      :headings '("Journal" "Tasks" "Ideas" "Links")
      )))

(cl-defun pm/note-read (&key (initial-input nil)
                             (filter-fn nil)
                             (sort-fn nil)
                             (require-match nil)
                             (prompt pm/note-find-prompt))
  (org-roam-node-read initial-input filter-fn sort-fn require-match prompt))

(cl-defun pm/note-find ()
  (interactive)
  (org-roam-node-visit (pm/note-read :require-match t) t))

(pm/leader
  "nf" '(pm/note-find :which-key "find note"))

(defvar pm/note-find-prompt "<[Note]> ")

(cl-defun pm/note-capture-new (&key node)
  (interactive)
  (if node
      (org-roam-capture-
       :goto nil
       :info nil
       :keys nil
       :templates `(("a" "Basic note" plain
                     ,pm/note-basic-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder)))
                    ("b" "Basic note + alias prompt" plain
                     ,pm/note-basic-entry
                     :target (file+head
                              ,pm/default-note-name-template
                              ,(pm/template-head-builder :aliases `("%^{ALIAS}"))))
                    ("c" "Basic note + tag prompt" plain
                     ,pm/note-basic-entry
                     :target (file+head
                              ,pm/default-note-name-template
                              ,(pm/template-head-builder :prompt-for-tags t)))
                    ("d" "Basic note + tag prompt + alias prompt" plain
                     ,pm/note-basic-entry
                     :target (file+head
                              ,pm/default-note-name-template
                              ,(pm/template-head-builder :prompt-for-tags t :aliases `("%^{ALIAS}")))))
       :node node
       :props '(:unnarrowed t :empty-lines-before 1))
    (user-error "Node cannot be nil!")))

(cl-defun pm/note-capture-existing (&key node)
  (interactive)
  (if node
      (org-roam-capture-
       :goto nil
       :info nil
       :keys nil
       :templates `(("a" "Edit note" plain
                     ,pm/note-basic-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder)))
                    ("b" "Edit note + goto" plain
                     ,pm/note-basic-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder))
                     :jump-to-captured t)
                    ("c" "Insert TODO" entry
                     ,pm/note-todo-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder))
                     :prepend t
                     :empty-lines 1)
                    ("d" "Insert TODO + goto" entry
                     ,pm/note-todo-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder))
                     :prepend t
                     :empty-lines 1
                     :jump-to-captured t)
                    ("e" "Insert IDEA" entry
                     ,pm/note-idea-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder))
                     :prepend t
                     :empty-lines 1)
                    ("f" "Insert IDEA + goto" entry
                     ,pm/note-idea-entry
                     :target (file+head ,pm/default-note-name-template ,(pm/template-head-builder))
                     :prepend t
                     :empty-lines 1
                     :jump-to-captured t))
       :node node
       :props '(:unnarrowed t)) ;; 
    (user-error "Node cannot be nil!")))

(cl-defun pm/note-capture ()
  (interactive)
  (let ((node (pm/note-read)))
    (if (org-roam-node-file node)
        (pm/note-capture-existing :node node)
      (pm/note-capture-new :node node))))

(pm/leader
  "nc" '(pm/note-capture :which-key "capture note"))

(cl-defun pm/chore-capture ()
  (interactive)
  (let (
        (headings '("House" "Car" "Emails"))
        (node (org-roam-populate (org-roam-node-create :title "Chores"))))
    (org-roam-capture-
     :goto nil
     :info nil
     :keys nil
     :templates `(("a" "House" entry
                   ,pm/note-todo-entry
                   :target (file+head+olp
                            "chore.org"
                            ,(pm/template-head-builder
                              :tags '("chore")
                              :headings headings)
                            ,'("House")))
                  ("b" "Car" entry
                   ,pm/note-todo-entry
                   :target (file+head+olp
                            "chore.org"
                            ,(pm/template-head-builder
                              :tags '("chore")
                              :headings headings)
                            ,'("Car")))
                  ("c" "Emails" entry
                   ,pm/note-todo-entry
                   :target (file+head+olp
                            "chore.org"
                            ,(pm/template-head-builder
                              :tags '("chore")
                              :headings headings)
                            ,'("Emails")))
                  ("d" "Clothes" entry
                   ,pm/note-todo-entry
                   :target (file+head+olp
                            "chore.org"
                            ,(pm/template-head-builder
                              :tags '("chore")
                              :headings headings)
                            ,'("Clothes"))))
     :node node
     :props '(:unnarrowed t))))

(pm/leader
  "nb" '(pm/chore-capture :which-key "capture chore"))

(cl-defun pm/url-from-clipboard (callback)
  (let ((url (org-cliplink-clipboard-content)))
    (if (url-type (url-generic-parse-url url)) 
        (org-cliplink-retrieve-title
         url
         callback)
      (user-error "Malformed url: %s" url))))

(cl-defun pm/link-read (ref)
  "Takes a string ref and either directly returns a node if a match is found, else nil. If multiple found, prompts for user selection between all of them"
  (save-match-data
    (let (type path)
      (cond
       ((string-match org-link-plain-re ref)
        (setq type (match-string 1 ref)
              path (match-string 2 ref)))
       ((string-prefix-p "@" ref)
        (setq type "cite"
              path (substring ref 1))))
      (when (and type path)
        (let ((ids (org-roam-db-query
                    [:select [nodes:id]
                             :from nodes
                             :join refs
                             :on (= nodes:id refs:node-id)
                             :where (= refs:type $s1)
                             :and (= refs:ref $s2)
                             ]
                    type path)))
          (cond
           ((= (length ids) 0) nil)
           ((= (length ids) 1) (org-roam-populate (org-roam-node-create :id (car ids))))
           (t (pm/note-read
               :prompt (format "Select from notes with ref: \"%s\": " ref)
               :filter-fn (lambda (node)
                            (cl-some (lambda (id)
                                       (string= (car id) (org-roam-node-id node)))
                                     ids))))))))))

(cl-defun pm/link-capture-new (url title)
  (let ((node (org-roam-node-create :title title)))
    (org-roam-capture-
     :goto nil
     :info nil
     :keys nil
     :templates `(("a" "Basic Link note" plain
                   ,pm/note-basic-entry
                   :target (file+head
                            ,pm/default-note-name-template
                            ,(pm/template-head-builder
                              :tags `("link")
                              :refs `(,url))))
                  ("b" "Basic Link note + tag prompt" plain
                   ,pm/note-basic-entry
                   :target (file+head
                            ,pm/default-note-name-template
                            ,(pm/template-head-builder
                              :prompt-for-tags t
                              :tags `("link")
                              :refs `(,url))))
                  ("c" "Bookmarked Link note" plain
                   ,pm/note-basic-entry
                   :target (file+head
                            ,pm/default-note-name-template
                            ,(pm/template-head-builder
                              :tags `("link" "bookmark")
                              :refs `(,url))))
                  ("d" "Bookmarked Link note + tag prompt" plain
                   ,pm/note-basic-entry
                   :target (file+head
                            ,pm/default-note-name-template
                            ,(pm/template-head-builder
                              :prompt-for-tags t
                              :tags `("link" "bookmark")
                              :refs `(,url)))))
     :node node
     :props '(:unnarrowed t))))


(cl-defun pm/link-capture ()
  (interactive)
  (pm/url-from-clipboard
   (lambda (url title)
     (if-let ((node (pm/link-read url)))
         (org-roam-node-visit node)
       (pm/link-capture-new url title)))))

(pm/leader
  "nl" '(pm/link-capture :which-key "capture link"))

(provide 'notes.el)
