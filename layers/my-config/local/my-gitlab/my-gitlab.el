(define-key gitlab-projects-mode-map (kbd "m") 'gitlab-mr-for-project)

(defun gitlab-mr-for-project (&optional project-id)
  "From projects buffer, opens MRs buffer for project at point."
  (interactive)
  (let ((project (gitlab-get-project (or project-id (tabulated-list-get-id)))))
    (if project
	(progn
	  (pop-to-buffer "*Gitlab MRs*" nil)
	  (gitlab-issues-mode)
	  (setq tabulated-list-entries
		(create-mr-entries (gitlab-list-project-mrs (assoc-default 'id project))))
	  (tabulated-list-print t)
	  (tabulated-list-sort 1))
      (user-error "No project here"))))

(define-derived-mode gitlab-mrs-mode tabulated-list-mode "Gitlab MRs"
  "Major mode for browsing Gitlab MRs."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("State" 10 t)
                               ("Project" 8 t)
                               ("Author" 20 t)
                               ("Assignee" 10 t)
                               ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

(defun gitlab-list-project-mrs (project-id &optional page per-page params)
  "Get a list of project issues.
PROJECT-ID : The ID of a project
PAGE: current page number
PER-PAGE: number of items on page max 100
PARAMS: an alist for query parameters. Exple: '((state . \"opened\"))"
  (let ((params params))
    (when page
      (add-to-list 'params (cons 'per_page (number-to-string per-page))))
    (when per-page
      (add-to-list 'params (cons 'page (number-to-string page))))
    (perform-gitlab-request "GET"
                            (s-concat "projects/"
                                      (url-hexify-string
                                       (format "%s" project-id))
                                      "/merge_requests")
                            params
                            200)))

(defun create-mr-entries (mrs)
  "Create entries for 'tabulated-list-entries from MRS."
  (mapcar (lambda (i)
            (let ((id (number-to-string (assoc-default 'id i)))
                  (author (assoc-default 'author i)))
              (list i
                    (vector ;id
                     (assoc-default 'state i)
                     (format "%s" (assoc-default 'project_id i))
                     (assoc-default 'name author)
                     (assoc-default 'title i)))))
          mrs))


(defun gitlab-list-users (&optional page per-page params)
  "Get a list of users
PAGE: current page number
PER-PAGE: number of items on page max 100
PARAMS: an alist for query parameters. Exple: '((state . \"opened\"))"
  (let ((params params))
    (when page
      (add-to-list 'params (cons 'per_page (number-to-string per-page))))
    (when per-page
      (add-to-list 'params (cons 'page (number-to-string page))))
    (perform-gitlab-request "GET"
                            "users"
                            params
                            200)))

(defun gitlab-get-user-name (users id)
  "Return users->name"
  (let (name)
    (mapcar (lambda (user)
              (if (= id (assoc-default 'id user))
                  (setq name (assoc-default 'name user))))
            users)
    name
    ))
