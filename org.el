;;; org.el --- Padraic's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2023-2023 Patrick H Morris

;; Author: Patrick H Morris <patrick.morris.310@gmail.com>
;; Keywords: internal
;; URL: https://github.com/Padraic-O-Mhuiris/emacs

;;; Commentary:
;; A fully fledged, reproducible Emacs configuration

;;; Code:

;; Inidcator to show content inside an org heading
(setq org-ellipsis " ⤵")
;; Indent according to nesting level
(setq org-startup-indented t)
;; Only show one heading marker
(setq org-hide-emphasis-markers t)
;; Fold all headings on startup
(setq org-startup-folded t)
;; Protects against editing internals of closed org-headings
(setq org-catch-invisible-edits t)

;; Always do indentation
(add-hook 'org-mode-hook 'org-indent-mode)
;; Always use bullets 
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Allows for simple elisp code blocks using "<el" + TAB
(add-to-list 'org-structure-template-alist
             '("el" . "src emacs-lisp"))

;; Always add a timestamp when a file is saved
(setq time-stamp-active t)
(setq time-stamp-start "#\\+last_modified:[ \t]")
(setq time-stamp-end "$")
(setq time-stamp-format "\[%Y-%m-%d %a %H:%M\]")
(add-hook 'before-save-hook #'time-stamp)

;;; source code blocks

;; Indent according to detected mode in a code block
(setq org-src-preserve-indentation t)
;; Don't ask to evaluate code blocks
(setq org-confirm-babel-evaluate nil)
;; Will tab as expected for that language
(setq org-src-tab-acts-natively t)

;; My org keywords
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

;; Add logbook for todo state transitions
(setq org-log-done 'note)
(setq org-log-into-drawer t)

;; Prevent todo state completion if it contains nested todos
(setq org-enforce-todo-dependencies t)

;; When adding a todo heading, always add a created property inactive timestamp
(defun pm/org-add-created-timestamp ()
  (save-excursion
    (org-back-to-heading)
    (org-set-property "CREATED"
                      (format-time-string
                       (org-time-stamp-format 'long 'inactive)
                       (org-current-time)))))
(add-hook 'pm/org-insert-todo-heading-hook 'pm/org-add-created-timestamp)
(defun pm/run-org-insert-todo-heading-hook (&rest _)
  (run-hooks 'pm/org-insert-todo-heading-hook))
(advice-add 'org-insert-todo-heading :after 'pm/run-org-insert-todo-heading-hook)
(advice-add 'org-insert-todo-heading-respect-content :after 'pm/run-org-insert-todo-heading-hook)
(advice-add 'org-insert-todo-subheading :after 'pm/run-org-insert-todo-heading-hook)

;; When inserting headings, is aware of content under point
(setopt org-insert-heading-respect-content t)
(setopt org-insert-todo-heading-respect-content t)

;;; Key binds
(pm/leader
  "o" '(:ignore t :which-key "org"))

(setopt org-directory "~/notes")

;;; Agenda
(setopt org-agenda-files (directory-files-recursively org-directory org-agenda-file-regexp))

(defun pm/org-agenda-menu ()
  (interactive)
  (org-agenda))

(pm/leader
  "oa" '(pm/org-agenda-menu :which-key "agenda"))

(setq org-agenda-restore-windows-after-quit t)


(provide 'org.el)
