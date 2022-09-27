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
(defvar timer-chk-user nil
  "use run-with-idle-timer to check if a user is in server")
(make-variable-buffer-local 'timer-chk-user)

(defun gitlab-list-project-mrs (project-id &optional page per-page params)
  "Get a list of project merge-requsts.
PROJECT-ID : The ID of a project
PAGE: current page number
PER-PAGE: number of items on page max 100
PARAMS: an alist for query parameters. Exple: '((state . \"opened\"))"
  (let ((params params)
        (mrs)
        (x-total-pages
         (string-to-number
          (gitlab-get-header (s-concat "projects/"
                                       (url-hexify-string
                                        (format "%s" project-id))
                                       "/merge_requests")
                             "X-Total-Pages"
                             200
                             page
                             per-page
                             params
                             ))))
    ;; (when page
    ;;   (add-to-list 'params (cons 'per_page (number-to-string per-page))))
    ;; (when per-page
    ;;   (add-to-list 'params (cons 'page (number-to-string page))))
    (loop do
          (setq mrs (vconcat mrs
                             (perform-gitlab-request "GET"
                                                     (s-concat "projects/"
                                                               (url-hexify-string
                                                                (format "%s" project-id))
                                                               "/merge_requests"
                                                               "?page=" (number-to-string page)
                                                               "&per_page=" (number-to-string per-page))
                                                     params
                                                     200)))
          (setq page (+ 1 page))
          while(<= page x-total-pages))
    mrs))

(defun create-mr-entries (mrs)
  "Create entries for 'tabulated-list-entries from MRS."
  (mapcar (lambda (i)
            (let ((id (number-to-string (assoc-default 'id i)))
                  (author (assoc-default 'author i))
                  (mr_status (assoc-default 'merge_status i))
                  (source_branch (assoc-default 'source_branch i))
                  (target_branch (assoc-default 'target_branch i))
                  (assignee (assoc-default 'assignee i)) )
              (list i ;; don't use i, only keep useful info in entriy's id
                    (vector ;id
                     (assoc-default 'state i)
                     mr_status
                     source_branch
                     target_branch
                     ;; performance, don't use project field
                     ;; (format "%s" (assoc-default 'name (gitlab-get-project (assoc-default 'project_id i))))
                     (assoc-default 'name author)
                     (or (and (not assignee)
                              "None")
                         ;; (gitlab-get-user-name-by-id (assoc-default 'id assignee))
                         (assoc-default 'username assignee))
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

(defun gitlab-update-mr ()
  "use widget to create a interface to initial parameters that requied by update a MR"
  (interactive)
  (let* ((cur-mr (tabulated-list-get-id))
         (project_id (or (assoc-default 'project_id cur-mr)
                         (assoc-default 'id gitlab-project)))
         (mr_iid (assoc-default 'iid cur-mr))
         (source (assoc-default 'source_branch cur-mr))
         (target (assoc-default 'target_branch cur-mr))
         (description (assoc-default 'description cur-mr))
         (assignee (assoc-default 'username (assoc-default 'assignee cur-mr)))
         (title (assoc-default 'title cur-mr)))
    (gitlab-create-mr-page project_id mr_iid title source target description assignee))
  )

(defun gitlab-create-mr-page (&optional project_id mr_iid title source target description assignee)
  "Create a widget buffer to get info of a gitlab MR.

PROJECT_ID: project ID
MR_IID: None-nil, update this MR; nil, create a new MR
"
  (interactive)
  (switch-to-buffer widget-mrs-buffer-name)
  (kill-all-local-variables)
  (make-local-variable 'widget-demo-form)
  (make-local-variable 'project_id)
  (make-local-variable 'mr_iid)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  ;; set default value that required by a MR
  (when (not mr_iid)
    (setq title "Draft: ")
    (setq source "master")
    (setq target "master")
    (setq assignee "")
    (setq description ""))
  ;; maybe nile
  (setq assignee (or assignee ""))
  (setq description (or description ""))

  (widget-insert "Create MR:\n\n")
  (widget-insert (format "Project ID: %s" project_id))
  (widget-insert "\n")
  (widget-mr-param-form-create
   'title
   (widget-create 'editable-field
                  :size 13
                  :format "Title: %v " ; Text after the field!
                  title))

  (widget-insert "\nDescription:")
  (widget-mr-param-form-create
   'description
   (widget-create 'editable-field
                  :size 13
                  :format " %v " ; Text after the field!
                  description
                  ))

  (widget-insert "\n")
  (widget-mr-param-form-create
   'source_branch
   (apply 'widget-create 'menu-choice
                   :tag "Source-Branch"
                   :value source
                   (mapcar (lambda (b)
                             (list 'item :tab b :value b))
                           (gitlab-list-project-branches-name (gitlab-list-project-branches project_id)))
                   ))
  (widget-mr-param-form-create
   'target_branch
   (apply 'widget-create 'menu-choice
          :tag "Target-Branch"
          :value target
          (mapcar (lambda (b)
                    (list 'item :tab b :value b))
                  (gitlab-list-project-branches-name (gitlab-list-project-branches project_id)))
          ))
  (widget-mr-param-form-create
   'assignee_id
   (widget-create 'editable-field
                  :format "Assignee: %v"
                  :value assignee
                  :notify (lambda (wid &rest ignore)
                            (let ((newassignee (widget-value wid)))
                              (when timer-chk-user
                                (cancel-timer timer-chk-user))
                              (setq timer-chk-user
                                    (run-with-idle-timer 1
                                                         nil
                                                         (lambda (newassignee)
                                                           (if (not (gitlab-get-user-id-by-username newassignee))
                                                               (message-box "Can't find Assignee in gitlab server:\n\t%s\t" newassignee)
                                                             (message "will assign to: %s" newassignee)
                                                             ))
                                                         newassignee))
                              ))
          ))
  (widget-insert "\n")
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let* ((params (mapcar (lambda (form)
                                                    (cons (car form) (widget-value (cdr form))))
                                                  widget-demo-form))
                                  (username (assoc-default 'assignee_id params)))
                             (setq params (assq-delete-all 'assignee_id params))
                             (add-to-list 'params (cons 'assignee_id (gitlab-get-user-id-by-username username)))
                             (gitlab-perform-create-mr project_id params mr_iid)
                             (kill-buffer widget-mrs-buffer-name)
                             ))
                 "Comment")
  (use-local-map widget-keymap)
  (widget-setup)
  )

(defun gitlab-perform-create-mr (project_id params &optional mr_iid)
  "Create a new MR"
  (perform-gitlab-request (if mr_iid "PUT" "POST")
                          (s-concat "projects/"
                                    (url-hexify-string
                                     (format "%s" project_id))
                                    "/merge_requests"
                                    (and mr_iid
                                         (format "/%s" mr_iid)))
                          params
                          (if mr_iid 200 201) )
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

(defun gitlab-refresh-mr-list ()
  "refresh current project's mr list"
  (interactive)
  (gitlab-mr-for-project (assoc-default 'id gitlab-project)))

(provide 'my-gitlab-mr)
