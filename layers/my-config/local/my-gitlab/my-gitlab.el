(define-key gitlab-projects-mode-map (kbd "m") 'gitlab-mr-for-project)

(defun gitlab-mr-for-project (&optional project-id)
  "From projects buffer, opens MRs buffer for project at point."
  (interactive)
  (let* ((project (gitlab-get-project (or project-id (tabulated-list-get-id))))
         (project-name (assoc-default 'name project)))
    (if project
	(progn
	  (pop-to-buffer (format "*Gitlab MRs[%s]*" project-name) nil)
	  (gitlab-mrs-mode)
	  (setq tabulated-list-entries
		(create-mr-entries (gitlab-list-project-mrs (assoc-default 'id project) 1 100 (list (cons "state" "opened")))))
	  (tabulated-list-print t)
	  (tabulated-list-sort 1))
      (user-error "No project here"))))

(define-derived-mode gitlab-mrs-mode tabulated-list-mode "Gitlab MRs"
  "Major mode for browsing Gitlab MRs."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("State" 10 t)
                               ("Merge_status" 20)
                               ("Source" 15)
                               ("Target" 15)
                               ("Project" 15 t)
                               ("Author" 15 t)
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
                  (author (assoc-default 'author i))
                  (mr_status (assoc-default 'merge_status i))
                  (source_branch (assoc-default 'source_branch i))
                  (target_branch (assoc-default 'target_branch i))
                  (assignee (assoc-default 'assignee i)) )
              (list i
                    (vector ;id
                     (assoc-default 'state i)
                     mr_status
                     source_branch
                     target_branch
                     (format "%s" (assoc-default 'name (gitlab-get-project (assoc-default 'project_id i))))
                     (assoc-default 'name author)
                     (or (and (not assignee)
                              "None")
                      (gitlab-get-user-name-by-id (assoc-default 'id assignee)))
                     (assoc-default 'title i)))))
          mrs))

(defun gitlab-get-user-name-by-id (id)
  "go through all pags to find user name"
  (let* ((per-page 100)
        (page 1)
        (x-total-pages (string-to-number (gitlab-get-header "users" "X-Total-Pages" 200 1 per-page)))
        (user (gitlab-get-user-name (gitlab-list-users page per-page) id))
        )
    (catch 'username
      (loop do
            (if user
                (throw 'username user)
              (setq page (1+ page))
              (setq user (gitlab-get-user-name (gitlab-list-users page per-page) id))
              )
            while (<= page x-total-pages))))
  )

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

(defun gitlab-get-header (uri key status-code &optional page per-page params)
  "return value of header of URI"
  (let ((params params)
        (response))
    (when per-page
      (add-to-list 'params (cons 'per_page (number-to-string per-page))))
    (when page
      (add-to-list 'params (cons 'page (number-to-string page))))
    (setq response (gitlab--perform-get-request uri params))
    (if (= status-code (request-response-status-code response))
        (request-response-header response key)
      (lwarn '(gitlab)
             :error "HTTP %s Error %s on URI: %s"
             "GET"
             (request-response-status-code response)
             uri)))
  )
