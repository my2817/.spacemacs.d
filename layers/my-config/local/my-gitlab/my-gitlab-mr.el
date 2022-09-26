(require 'my-gitlab-user)
(require 'my-gitlab-branche)

(eval-when-compile
  (require 'cl))
(require 'info)
(require 'tree-widget)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar widget-demo-form nil
  "A table for lookup widget created in current buffer.")

(defvar widget-mrs-buffer-name "*Creat MRs*"
  "*Name of the widget demo buffer")

(defun gitlab-list-project-mrs (project-id &optional page per-page params)
  "Get a list of project merge-requsts.
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

(defun widget-mr-param-form-create (id widget)
  (if (assoc id widget-demo-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) widget-demo-form)))

(defun gitlab-create-mr ()
  "use widget to create a interface of MR requied parameters"
  (interactive)
  (let* ((project_id (or (assoc-default 'project_id (tabulated-list-get-id))
                         (assoc-default 'id gitlab-project))))
    (gitlab-create-mr-page project_id))
  )

(defun gitlab-create-mr-page (&optional project_id)
  "Create a widget buffer to get info of a gitlab MR."
  (interactive)
  (switch-to-buffer widget-mrs-buffer-name)
  (kill-all-local-variables)
  (make-local-variable 'widget-demo-form)
  (make-local-variable 'project_id)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Create MR:\n\n")
  (widget-insert (format "Project ID: %s" project_id))
  (widget-insert "\n")
  (widget-mr-param-form-create
   'title
   (widget-create 'editable-field
                  :size 13
                  :format "Title: %v " ; Text after the field!
                  "Draft: "))

  (widget-insert "\n")
  (widget-mr-param-form-create
   'source_branch
   (apply 'widget-create 'menu-choice
                   :tag "Source-Branch"
                   :value "master"
                   (mapcar (lambda (b)
                             (list 'item :tab b :value b))
                           (gitlab-list-project-branches-name (gitlab-list-project-branches project_id)))
                   ))
  (widget-mr-param-form-create
   'target_branch
   (apply 'widget-create 'menu-choice
          :tag "Target-Branch"
          :value "master"
          (mapcar (lambda (b)
                    (list 'item :tab b :value b))
                  (gitlab-list-project-branches-name (gitlab-list-project-branches project_id)))
          ))
  (widget-mr-param-form-create
   'assignee_id
   (widget-create 'editable-field
                  :format "Assignee: %v"
          ))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let* ((params (mapcar (lambda (form)
                                                    (cons (car form) (widget-value (cdr form))))
                                                  widget-demo-form))
                                  (username (assoc-default 'assignee_id params)))
                             (setq params (assq-delete-all 'assignee_id params))
                             (add-to-list 'params (cons 'assignee_id (gitlab-get-user-id-by-username username)))
                             (gitlab-perform-create-mr project_id params)
                             (kill-buffer widget-mrs-buffer-name)
                             ))
                 "Comment")
  (use-local-map widget-keymap)
  (widget-setup)
  )

(defun gitlab-perform-create-mr (project_id params)
  "Create a new MR"
  (perform-gitlab-request "POST"
                          (s-concat "projects/"
                                    (url-hexify-string
                                     (format "%s" project_id))
                                    "/merge_requests")
                          params
                          201 )
  )

(defun gitlab-close-mr ()
  "close mr at point"
  (interactive)
  (let* ((mr (tabulated-list-get-id)))
    (gitlab-edit-mr (assoc-default 'project_id mr) (assoc-default 'iid mr) nil nil nil nil nil "close"))
  )

(defun gitlab-reopen-mr ()
  "reopen mr at point"
  (interactive)
  (let* ((mr (tabulated-list-get-id)))
    (gitlab-edit-mr (assoc-default 'project_id mr) (assoc-default 'iid mr) nil nil nil nil nil "reopen"))
  )

(defun gitlab-edit-mr (project-id issue-iid &optional title description assignee-id milestone-id labels state-event)
  "edit a project mr.

PROJECT-ID the ID or NAMESPACE%2FPROJECT_NAME of a project
ISSUE-IID : The ID of a project issue
TITLE issue title
DESCRIPTION issue description
ASSIGNEE-ID assignee ID
MILESTONE-ID milestone ID
LABELS comma-separated list label names"
  (lwarn '(gitlab) :debug "UPDATE MR in project: %s\n" project-id)
  (perform-gitlab-request "PUT"
                          (format "projects/%s/merge_requests/%s"
                                   (url-hexify-string
                                    (format "%s" project-id))
                                   issue-iid)

                          (format "%s"
                                  (concat
                                   (when title
                                     (format "&title=%s" title))
                                   (when description
                                     (format "&description=%s" description))
                                   (when assignee-id
                                     (format "&assignee_id=%s" assignee-id))
                                   (when milestone-id
                                     (format "&milestone_id=%s" milestone-id))
                                   (when labels
                                     (format "&labels=%s" labels))
                                   (when state-event
                                     (format "&state_event=%s" state-event))))
                          200))

(defun gitlab-merge-mr ()
  "merge mr at point"
  (interactive)
  (let* ((mr (tabulated-list-get-id))
         (project-id (assoc-default 'project_id mr))
         (mr-iid (assoc-default 'iid mr)))
    (perform-gitlab-request "PUT"
                            (format "projects/%s/merge_requests/%s/merge"
                                    (url-hexify-string
                                     (format "%s" project-id))
                                    mr-iid)
                            nil
                            200)))

(provide 'my-gitlab-mr)
