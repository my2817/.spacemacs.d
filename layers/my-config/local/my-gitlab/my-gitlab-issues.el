(require 'gitlab)

;; duplicate define funcs
(define-derived-mode gitlab-issues-mode tabulated-list-mode "Gitlab issues"
  "Major mode for browsing Gitlab issues."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("State" 10 t)
                               ("Project" 8 t)
                               ("Author" 20 t)
                               ("Assignee" 20 t)
                               ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

(defun create-issues-entries (issues)
  "Create entries for 'tabulated-list-entries from ISSUES."
  (mapcar (lambda (i)
            (let ((id (number-to-string (assoc-default 'id i)))
                  (author (assoc-default 'author i))
                  (assignee (assoc-default 'assignee i)))
              (list i
                    (vector ;id
                     (assoc-default 'state i)
                     (format "%s" (assoc-default 'project_id i))
                     (assoc-default 'name author)
                     (or (and (not assignee)
                              "None")
                         ;; (gitlab-get-user-name-by-id (assoc-default 'id assignee))
                         (assoc-default 'name assignee))
                     (assoc-default 'title i)))))
          issues))

(defun gitlab-show-issues-current-project ()
  "Show issues of current project, `projectile-project-name' to get project name"
  (interactive)
  (gitlab-issues-for-project (gitlab-get-project-id-by-name (projectile-project-name)))
  )

(provide 'my-gitlab-issues)
