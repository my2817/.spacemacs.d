(eval-when-compile
  (require 'cl))
(require 'info)
(require 'tree-widget)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))


(defvar widget-demo-form nil
  "A table for lookup widget created in current buffer.")

(defvar widget-demo-buffer-name "*Creat MRs*"
  "*Name of the widget demo buffer")


(defun widget-demo-form-create (id widget)
  (if (assoc id widget-demo-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) widget-demo-form)))

(defun my-gitlab-create-mr-page (&optional project_id)
  "Create a widget buffer to get info of a gitlab MR."
  (interactive)
  (switch-to-buffer "*Create MR*")
  (kill-all-local-variables)
  (make-local-variable 'widget-demo-form)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Create MR:\n\n")
  (widget-insert (format "Project ID: %s" project_id))

  (widget-demo-form-create
   'title
   (widget-create 'editable-field
                  :size 13
                  :format "Title: %v " ; Text after the field!
                  "fuck"))

  (widget-insert "\n")
  (widget-demo-form-create
   'source_branch
   (apply 'widget-create 'menu-choice
                   :tag "Source-Branch"
                   :value "master"
                   (mapcar (lambda (b)
                             (list 'item :tab b :value b))
                           (magit-list-local-branch-names))
                   ))
  (widget-demo-form-create
   'target_branch
   (apply 'widget-create 'menu-choice
          :tag "Target-Branch"
          :value "master"
          (mapcar (lambda (b)
                    (list 'item :tab b :value b))
                  (magit-list-local-branch-names))
          ))
  (widget-demo-form-create
   'assignee_id
   (widget-create 'editable-field
                  :format "Assignee: %v"
          ))
  (widget-create 'push-button
                 :notify (lambda (&rest ignore)
                           (let* ((params (mapcar (lambda (form)
                                                    (cons (car form) (widget-value (cdr form))))
                                                  widget-demo-form))
                                  (username (assoc-default "assignee_id" params)))
                             (setq params (assoc-delete-all "assignee_id" params))
                             (add-to-list 'params (cons "assignee_id" (gitlab-get-user-id-by-username username)))
                             (my-gitlab-create-mr project_id params)))
                 "Comment")
  (use-local-map widget-keymap)
  (widget-setup)
  )

(defun my-gitlab-create-mr (project_id params)
  "fuck"
  (ignore)
  )


(defun gitlab-get-user-id-by-username (username)
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
              (setq user (gitlab-get-user-id (gitlab-list-users page per-page) username))
              )
            while (<= page x-total-pages))))
  )

(defun gitlab-get-user-id (users username)
  "Return users->name"
  (let (id)
    (mapcar (lambda (user)
              (if (string= username (assoc-default 'username user))
                  (setq id (assoc-default 'id user))))
            users)
    id
    ))
