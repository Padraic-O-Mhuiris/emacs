(defun pm/intersperse (lst value)
  "Intersperse LST with VALUE."
  (if (null lst)
      '()
    (let ((rest-of-list (cdr lst)))
      (if (null rest-of-list)
          lst
        (cons (car lst) (cons value (pm/intersperse rest-of-list value)))))))

(defun pm/list-to-org (lst &optional level)
  (unless level (setq level 1))
  (mapconcat (lambda (item)
               (if (listp item)
                   (pm/list-to-org item (1+ level))
                 (concat (make-string level ?*) " " item "\n\n")))
             lst
             ""))

(cl-defun pm/list2str (lst &key (between "") (before "") (after ""))
  (apply #'concat `(,before ,(apply #'concat (pm/intersperse (cl-remove-if-not 'stringp lst) between)) ,after)))

(cl-defun pm/template-head-builder (&key (title "${title}")
                                         (tags `())
                                         (aliases `())
                                         (refs `())
                                         (headings `())
                                         (prompt-for-tags nil)
                                         (project-path nil)
                                         (created "#+created_at: %U")
                                         (modified  "#+last_modified: %U"))
  "This function is the default builder for all note templates. It expects a series of keys and values in each case:
"
  (let
      ((file-tags (if (or
                       (> (length tags) 0)
                       prompt-for-tags)
                      (pm/list2str tags
                                   :before (concat "#+filetags: "
                                                   (if (> (length tags) 0) ":" ""))
                                   :after (concat (if (> (length tags) 0) ":" "")
                                                  (if prompt-for-tags "%^G" ""))
                                   :between ":")
                    nil))
       (properties (if (or
                        (> (length refs) 0)
                        (> (length aliases) 0))
                       (pm/list2str
                        `(,(when (> (length aliases) 0)
                             (pm/list2str aliases :between " " :before ":ROAM_ALIASES: " :after "\n"))
                          ,(when (> (length refs) 0)
                             (pm/list2str refs :between " " :before ":ROAM_REFS: " :after "\n")))
                        :before ":PROPERTIES:\n"
                        :after ":END:"
                        :between "\n")
                     nil))
       (olp (if (> (length headings) 0)
                (concat "\n" (pm/list-to-org headings))
              nil)))
    (pm/list2str `(,properties
                   ,(concat "#+title: " title)
                   ,created
                   ,modified
                   ,(when project-path (concat "#+project_path: " project-path))
                   ,file-tags
                   ,olp
                   )
                 :between "\n")))

(cl-defun pm/template-entry-builder (&key (todo-state nil)
                                          (title-content nil)
                                          (entry-content nil)
                                          (levels 0)
                                          (tags `())
                                          (no-properties nil))
  (let
      ((tag-str (if (> (length tags) 0)
                    (pm/list2str tags :between ":" :before " :" :after ":")
                  nil))
       (levels-str (if (> levels 0)
                       (make-string levels ?*)
                     nil)))
    (pm/list2str `(,(when (> levels 0) (pm/list2str `(,levels-str
                                                      ,todo-state
                                                      ,title-content
                                                      ,tag-str)
                                                    :between " "))
                   ,(unless no-properties ":PROPERTIES:\n:CREATED:  %U\n:END:")
                   ,(when entry-content entry-content))
                 :between "\n")))

(defun display-startup-time ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

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

(defun pm/reload-config ()
  "Reloads the emacs configuration"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))


(defun pm/get-org-keywords-from-file (file keywords)
  "Collect values of KEYWORDS from an Org-mode FILE.
If KEYWORDS is nil, collect all buffer-wide settings."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-collect-keywords keywords)))

(provide 'functions.el)
;;; functions.el ends here
