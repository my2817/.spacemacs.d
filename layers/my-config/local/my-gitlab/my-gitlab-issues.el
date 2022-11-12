(require 'gitlab)

(define-key gitlab-issues-mode-map (kbd "R") 'gitlab-issues-refresh)
(define-key gitlab-issues-mode-map (kbd "c") 'gitlab-issues-create)
(define-key gitlab-issues-mode-map (kbd "u") 'gitlab-issues-modi)
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

;; save project to local
(defun gitlab-issues-for-project (&optional project-id)
  "From projects buffer, opens issues buffer for project at point."
  (interactive)
  (let ((project (gitlab-get-project (or project-id (tabulated-list-get-id)))))
    (if project
	(progn
	  (pop-to-buffer "*Gitlab issues*" nil)
          (make-local-variable 'gitlab-project)
	  (gitlab-issues-mode)
          (setq-local gitlab-project project)
	  (setq tabulated-list-entries
		(create-issues-entries (gitlab-list-project-issues (assoc-default 'id project))))
	  (tabulated-list-print t)
	  (tabulated-list-sort 1))
      (user-error "No project here"))))

(defun gitlab-issues-refresh ()
  "refresh current project's issues list"
  (interactive)
  (gitlab-issues-for-project (assoc-default 'id gitlab-project)))

(defun gitlab-issues-create ()
  "create a new issue"
  (interactive)
  (let* ((project-id (assoc-default 'id gitlab-project)))
    (gitlab-issue-edit project-id nil)
    ))

(defun gitlab-issues-do-put ()
  "DO `gitlab-issue-put'"
  (interactive)
  (let ()
    (gitlab-issues-put (assoc-default 'project-id gitlab-info)
                       (gitlab-issue-parse-buffer)
                       (assoc-default 'issue-id gitlab-info))
    (kill-buffer-and-window)))

(defun gitlab-issues-modi ()
  "Edit issue's title/assignee/description at cursor"
  (interactive)
  (let* ((project-id (assoc-default 'project_id (tabulated-list-get-id)))
         (issue-iid (assoc-default 'iid (tabulated-list-get-id))) )
    (gitlab-issue-edit project-id issue-iid)
    )
  )

(defun gitlab-issues-put (project_id params &optional iid)
  "create a new issue"
  (perform-gitlab-request (if iid "PUT" "POST")
                          (s-concat "projects/"
                                    (url-hexify-string
                                     (format "%s" project_id))
                                    "/issues"
                                    (and iid
                                         (format "/%s" iid)))
                          params
                          (if iid 200 201) )
  )
(defun gitlab-issue-edit (&optional project-id issue-id)
  "create or edit a issue.

PROJECT-ID: project-id
ISSUE-ID: nil to create a new ISSUE; or update a issue"
  (let* (
         (issue (and issue-id
                     (gitlab-get-issue project-id issue-id)))
         (title (or (and issue-id
                         (assoc-default 'title issue))
                    (format "TITLE")))
         (description (or (and issue-id
                               (assoc-default 'description issue))
                          (format "DESCRIPTION")))
         (assignee (or (and issue-id
                            (assoc-default 'name (assoc-default 'assignee issue)))
                       (format "ASSIGNEE")))
         (gitlab-info )
         )
    (pop-to-buffer (format "*Gitlab Edit Issue:[#%s]*" (or issue-id
                                                          "new")) nil)
    (gitlab-edit-note-mode)
    (use-local-map (let ((map (copy-keymap markdown-mode-map)))
                     ;; (define-key map (kbd "v") 'print-current-issue-id)
                     ;; (define-key map (kbd "w") 'gitlab-goto-issue)
                     (define-key map (kbd "C-c C-c" ) 'gitlab-issues-do-put)
                     map) )
    (setq-local gitlab-project-id project-id)
    (page-break-lines-mode)
    (setq header-line-format
          (concat (propertize " " 'display '((space :align-to 0)))
                  (format "[%s] C-c C-c: finish Edit; Kill Buffer: Abort\n\nMarkDown: Don't remove TAG and line-break"
                          (projectile-project-name))))
    (make-local-variable 'gitlab-info)
    (setq-local gitlab-info (list (cons 'project-id project-id)
                                  (cons 'issue-id issue-id)))
    (dolist (item (list 'title 'assignee 'description))
            (insert (format "%s:\n=\n%s\n\n\n" item (eval item))))
    ))

(defun gitlab-issue-parse-buffer ()
  "Collection issue information in buffer which created by `gitlab-issue-edit'"
  (goto-char (point-min))
  (let* ((title-st (progn
                     (search-forward "title:\n=\n" (point-max))
                     (point)
                     ))
         (title (buffer-substring-no-properties (point) (point-at-eol)))
         (assignee-st (progn
                        (search-forward "assignee:\n=\n" (point-max))
                        (point)
                        ))
         (assignee (buffer-substring-no-properties (point) (point-at-eol)))
         (description-st (progn
                           (search-forward "description:\n=\n" (point-max))
                           (point)
                           ))
         (description (buffer-substring-no-properties (point) (- (point-max) 2)))
         )
    (list (cons 'title title)
          (cons 'assignee_ids (gitlab-get-user-id-by-username assignee))
          (cons 'description description))
    ))

(provide 'my-gitlab-issues)
