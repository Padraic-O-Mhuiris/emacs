;;; links.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

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

(provide 'links.el)
