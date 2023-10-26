;;; config.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://panadestein.github.io/emacsd/

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

(load-file (concat user-emacs-directory "functions.el"))

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar pm/gc-cons-threshold 100000000)

(add-hook 'emacs-startup-hook ; hook run after loading init files
          (lambda ()
            (setq gc-cons-threshold pm/gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)))

(add-hook 'minibuffer-setup-hook
          #'(lambda ()
              (setq gc-cons-threshold (* pm/gc-cons-threshold 2))))
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (garbage-collect)
              (setq gc-cons-threshold pm/gc-cons-threshold)))

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq native-comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

(defvar pm/initialized nil)

(add-hook 'emacs-startup-hook
          #'(lambda ()
              (setq pm/initialized t)))

(setq user-full-name "Patrick H Morris"
      user-mail-address "patrick.morris.310@gmail.com")

(setq inhibit-default-init t)

(let ((customization-file
       (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p customization-file)
    (write-region "" nil customization-file))
  (setq custom-file customization-file)
  (load custom-file 'noerror))

(setq inhibit-startup-message t)

(defun display-startup-echo-area-message ()
  (display-startup-time))

(setq initial-buffer-choice "~/.config/emacs/config.org")

(setq initial-scratch-message nil)

(setq large-file-warning-threshold 100000000)

(global-auto-revert-mode t)
(setq auto-revert-interval 1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq make-backup-files nil)

(setq sentence-end-double-space nil)

(defun pm/suppress-save-buffer-query-function ()
  (set-buffer-modified-p nil)
  t) ; Return t so other functions in kill-buffer-query-functions get called.

(add-to-list 'kill-buffer-query-functions 'pm/suppress-save-buffer-query-function)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)

(setq ring-bell-function 'ignore)

(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(fset 'yes-or-no-p 'y-or-n-p)

(if init-file-debug
    (setq warning-minimum-level :debug)
  (setq warning-minimum-level :emergency))

(require 'savehist)
(savehist-mode)

(require 'no-littering)

(require 'general)
(general-evil-setup t)

(general-create-definer pm/leader
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0)

(pm/leader
  "r" '(pm/reload-config :which-key "Reload config")
  "u" '(:ignore t :which-key "ui")
  "ut" '(counsel-load-theme :which-key "Select Theme"))

(general-define-key
 "<escape>" 'keyboard-escape-quit)

(require 'doom-themes)
(setq doom-themes-enable-bold t    
      doom-themes-enable-italic t)

(unless pm/initialized (load-theme 'doom-nord-aurora t)) 
(doom-themes-visual-bell-config)
(doom-themes-org-config)

(set-face-attribute 'default nil :font "Iosevka Comfy Fixed" :height 100)

(require 'all-the-icons)

(require 'doom-modeline)
(doom-modeline-mode t)
(setq doom-modeline-height 55)
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-line-numbers-style 'relative)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-window-width-limit nil)

(set-fringe-mode 0)

(require 'winner)
(winner-mode t)

(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(require 'so-long)

(add-hook 'after-init-hook 'global-so-long-mode)

(setq fill-column 80)

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'prog-mode-hook 'visual-line-mode)

(require 'visual-fill-column)
(add-hook 'visual-line-mode-hook
          #'(lambda ()
              (setq visual-fill-column-width 140) 
              (visual-fill-column-mode)))

(setq mode-line-right-align-edge 'right-fringe)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)

(setq-default tab-always-indent 'complete)

(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-i-jump nil)
(setq evil-undo-system 'undo-tree)

(require 'evil)
(evil-mode 1)

(require 'evil-collection)
(evil-collection-init)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(general-def 'evil-insert-state-map
  "C-g" 'evil-normal-state
  "C-h" 'evil-delete-backward-char-and-join)

(general-define-key 
 :states 'motion
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line)

(require 'undo-tree)
(general-define-key
 "C-x u" 'undo-tree-visualize)

(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(setq undo-tree-show-help-in-visualize-buffer t)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo"))))

(require 'magit)
(setopt magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1) ; What does this do?

(pm/leader
  "m" '(:ignore t :which-key "magit")
  "mm" '(magit-status :which-key "status"))

(require 'org)

(pm/leader
  "o" '(:ignore t :which-key "org"))

(setq org-ellipsis " ⤵")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq org-startup-indented t)
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-hide-emphasis-markers t)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist
             '("el" . "src emacs-lisp"))

(setq org-src-preserve-indentation t)

(setq org-src-tab-acts-natively t)

(setq org-confirm-babel-evaluate nil)

(setq org-startup-folded t)

(require 'time-stamp)

(setq time-stamp-active t)
(setq time-stamp-start "#\\+last_modified:[ \t]")
(setq time-stamp-end "$")
(setq time-stamp-format "\[%Y-%m-%d %a %H:%M\]")

(add-hook 'before-save-hook #'time-stamp)

(require 'org-cliplink)

(setq ob-mermaid-cli-path (getenv "MERMAID_CLI"))
(add-to-list 'org-babel-load-languages '(mermaid . t))

(add-to-list 'org-structure-template-alist
             '("mrm" . "src mermaid :file /tmp/test.png"))

(setq org-catch-invisible-edits t)

(setq org-todo-keywords
      '((sequence
         "TODO(t)" ;; A task that needs doing and is ready to do
         "PROG(p!)" ;; A task that is in progress
         "NEXT(n!)" ;; A task which should be done next
         "WAIT(w@/!)" ;; A task which is held up for an external reason
         "HOLD(h@/!)" ;; A task which is paused
         "|" ;;
         "DONE(d!)" ;; When a task is completed
         "KILL(k@/!)" ;; When a task is rejected
         "FAIL(f@/!)" ;; When a task is failed
         )
        (sequence
         "NOTE" ;; Not necessary for agenda, just for highlighting in places
         "LINK" ;; A naked url which is to be changed to a link note
         "IDEA" ;; A piece of information which might manifest into something
         "|")))

(setq org-log-done 'note)
(setq org-log-into-drawer t)

(setq org-enforce-todo-dependencies t)

(defun pm/run-org-insert-todo-heading-hook (&rest _)
  "Run `pm/org-insert-todo-heading-hook'."
  (run-hooks 'pm/org-insert-todo-heading-hook))

(advice-add 'org-insert-todo-heading :after 'pm/run-org-insert-todo-heading-hook)
(advice-add 'org-insert-todo-heading-respect-content :after 'pm/run-org-insert-todo-heading-hook)
(advice-add 'org-insert-todo-subheading :after 'pm/run-org-insert-todo-heading-hook)

;; (add-hook 'org-after-todo-state-change-hook #'my/log-todo-creation-date)

(setopt org-insert-heading-respect-content t)
(setopt org-insert-todo-heading-respect-content t)

(require 'org-expiry)

(defun pm/org-add-created-timestamp ()
  (save-excursion
    (org-back-to-heading)
    (org-set-property "CREATED"
                      (format-time-string
                       (org-time-stamp-format 'long 'inactive)
                       (org-current-time)))))

(add-hook 'pm/org-insert-todo-heading-hook 'pm/org-add-created-timestamp)

;; (defun pm/org-todo-heading-add-ordered-property ()
;;   (when (org-at-heading-p)
;;     (org-set-property "ORDERED" "t")))

;; (add-hook 'pm/org-insert-todo-heading-hook 'pm/org-todo-heading-add-ordered-property)

(require 'org-roam)
(setopt org-directory "~/notes"
        org-roam-directory org-directory
        org-roam-dailies-directory "daily/")
(setq org-roam-database-connector 'sqlite-builtin)
(org-roam-db-autosync-mode)

(pm/leader
  "n" '(:ignore t :which-key "notes"))

(defun pm/return-t (orig-fun &rest args) t)
(defun pm/disable-yornp (orig-fun &rest args)
  (advice-add 'yes-or-no-p :around #'pm/return-t)
  (advice-add 'y-or-n-p :around #'pm/return-t)
  (let ((res (apply orig-fun args)))
    (advice-remove 'yes-or-no-p #'pm/return-t)
    (advice-remove 'y-or-n-p #'pm/return-t)
    res))
(advice-add 'org-roam-capture--finalize :around #'pm/disable-yornp)

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

(defvar pm/note-basic-entry (pm/template-entry-builder :entry-content "%?" :no-properties t))

(defvar pm/note-todo-entry (pm/template-entry-builder :todo-state "TODO" :levels 2 :title-content "%?"))

(defvar pm/note-journal-entry (pm/template-entry-builder :title-content "[%<%T>]\n %?" :levels 2 :no-properties t))

(defvar pm/note-idea-entry (pm/template-entry-builder :todo-state "IDEA" :levels 2 :title-content "%?"))

(defvar pm/note-link-entry (pm/template-entry-builder
                            :no-properties t
                            :title-content "%(org-cliplink-capture)"))

(defvar pm/default-note-name-template "%<%s>__${slug}.org")

(defvar pm/project-note-name-template "project/${slug}.org")

(defvar pm/daily-note-name-template "%<%Y-%m-%d>.org")

(defvar pm/people-note-name-template "people/<%s>__${slug}.org")

(defvar pm/basic-note-target `(file+head ,pm/default-note-name-template ,(pm/template-head-builder)))

(defvar pm/action-note-target
  `(file+head
    ,pm/default-note-name-template
    ,(pm/template-head-builder
      :headings '("Journal" "Tasks" "Ideas" "Links")
      )))

(defvar pm/basic-note-template
  `("d" "" plain
    pm/note-basic-entry
    :target ,pm/basic-note-target
    :unnarrowed t
    :empty-lines-before 1))

(defvar pm/note-find-prompt "<[Note]> ")

(pm/leader
  "nf" '(:ignore t :which-key "find notes"))

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
                            ,'("Emails"))))
     :node node
     :props '(:unnarrowed t))))


;; (cl-defun pm/project-capture ()
;;   (interactive)
;;   (let ((node (pm/project-read)))
;;     (if (org-roam-node-file node)
;;         (pm/project-capture-existing node)
;;       (pm/project-capture-new node))))

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

(cl-defun pm/project-read () 
  (pm/note-read
   :prompt "Select from projects: "
   :filter-fn (lambda (node)
                (cl-some (lambda (tag)
                           (string= tag "project"))
                         (org-roam-node-tags node)))))
(defun project-capture-templates ()
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
     ,(pm/template-entry-builder :title-content (concat "[ " (pm/todays-date) " ]")
                                 :entry-content (concat "*** [ " (pm/current-time) " ]\n%?")
                                 :no-properties t
                                 :levels 2)
     :target (file+head+olp
              ,pm/project-note-name-template
              ,(pm/template-head-builder
                :tags '("project")
                :headings '("Abstract" "Tasks" "Ideas" "Links" "Journal"))
              ,'("Journal")))))

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

(pm/leader
  "np" '(pm/project-capture :which-key "capture project"))

(require 'ts)

(cl-defun pm/todays-date ()
  (let* ((now (ts-now))
         (day (ts-day now))
         (suffix (cond ((memq day '(11 12 13)) "th")
                       ((= 1 (% day 10)) "st")
                       ((= 2 (% day 10)) "nd")
                       ((= 3 (% day 10)) "rd")
                       (t "th"))))
    (concat (ts-day-name now)
            ", "
            (format "%s" (ts-day-of-month-num now))
            suffix
            " of "
            (format "%s" (ts-month-name now))
            " "
            (format "%s" (ts-year now))
            )))

(cl-defun pm/current-time ()
  (let* ((now (ts-now))
         (hour (ts-H now))
         (minute (ts-M now))
         (hour-formatted (if (< hour 10)
                             (format "0%s" hour)
                           (format "%s" hour)))
         (minute-formatted (if (< minute 10)
                             (format "0%s" minute)
                           (format "%s" minute))))
      (concat hour-formatted ":" minute-formatted)))


;; (pm/current-time)

(setopt org-agenda-files (directory-files-recursively org-directory org-agenda-file-regexp))

(defun pm/org-agenda-menu ()
  (interactive)
  (org-agenda))

(pm/leader
  "oa" '(pm/org-agenda-menu :which-key "agenda"))

(setq org-agenda-restore-windows-after-quit t)

(require 'projectile)
(projectile-mode +1)

(add-to-list 'projectile-globally-ignored-directories "/nix/*")

(setq projectile-project-search-path
      '(
        "~/.config/emacs"
        "~/notes"
        ("~/code" . 4)))

(pm/leader
  "p" '(:ignore t :which-key "switch project")
  "pp" '(projectile-switch-project :which-key "switch project")
  "pf" '(projectile-find-file :which-key "find project file")
  "pb" '(projectile-switch-to-buffer :which-key "find project buffer")
  ;; ... add other projectile-specific bindings as needed
  )
(setq projectile-sort-order 'recentf)
(setq projectile-per-project-compilation-buffer t)
;; (use-package projectile  
;;   :straight t
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (when (file-directory-p "~/code")
;;     (setq projectile-project-search-path '("~/code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; (use-package counsel-projectile  
;;   :straight t
;;   :after projectile
;;   :config (counsel-projectile-mode))

(require 'vertico)
(vertico-mode)

;; Different scroll margin
(setq vertico-scroll-margin 0)

;; Show more candidates
(setq vertico-count 20)

;; Grow and shrink the Vertico minibuffer
(setq vertico-resize t)

;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
(setq vertico-cycle t)

(require 'corfu)

(global-corfu-mode)
(setq corfu-auto t
      corfu-quit-no-match 'separator)

(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'consult)
(require 'consult-projectile)
(require 'consult-org-roam)
(require 'consult-notes)
(require 'consult-dir)

(require 'marginalia)
(marginalia-mode)

(require 'embark)
(require 'embark-consult)

(general-define-key
 "C-." 'embark-act
 "C-;" 'embark-dwim
 "C-h B" 'embark-bindings)

;; Optionally replace the key help with a completing-read interface
(setq prefix-help-command #'embark-prefix-help-command)
;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
;; strategy, if you want to see the documentation from multiple providers.
(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)


(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)

(setq display-line-numbers-width-start t)

(require 'smartparens)
(add-hook 'prog-mode-hook 'smartparens-mode)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(require 'esup)

(require 'helpful)

(general-define-key
 "C-h f" 'helpful-function
 "C-h v" 'helpful-variable
 "C-h k" 'helpful-key)

(defun pm/reload-config ()
  "Reloads the emacs configuration"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(provide 'config.el)
;;; config.el ends here
