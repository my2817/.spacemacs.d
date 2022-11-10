(require 'gitlab)
(require 'markdown-mode)

(define-key gitlab-issues-mode-map (kbd "RET") 'gitlab-show-issue-notes-current)
(define-key gitlab-mrs-mode-map (kbd "RET") 'gitlab-show-mr-notes-current)

(defvar gitlab-notes-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    ;; (define-key map (kbd "v") 'print-current-issue-id)
    ;; (define-key map (kbd "w") 'gitlab-goto-issue)
    (define-key map (kbd "RET") 'gitlab-new-note)
    (define-key map (kbd "q") 'kill-buffer-and-window)
    (define-key map (kbd "r") 'gitlab-refresh-note)
    map)
  "Keymap for `gitlab-notes-mode' major mode.")

(define-derived-mode gitlab-notes-mode markdown-mode "Gitlab Notes"
  "major mode for dispaly notes"
  :group 'gitlab
  (use-local-map gitlab-notes-mode-map))

(defvar gitlab-edit-note-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    ;; (define-key map (kbd "v") 'print-current-issue-id)
    ;; (define-key map (kbd "w") 'gitlab-goto-issue)
    (define-key map (kbd "C-c C-c" ) 'gitlab-comment-new-note)
    map)
  "Keymap for `gitlab-edit-note-mode' major mode.")

(define-derived-mode gitlab-edit-note-mode markdown-mode "Gitlab Edit Notes"
  "major mode for edit note body"
  :group 'gitlab
  (use-local-map gitlab-edit-note-mode-map))

(defun gitlab-list-notes (project-id gitlab-note-to gitlab-note-to-iid &optional page per-page params)
  "Get a list of issue notes
PROJECT-ID : The ID of a project
GITLAB-NOTE-TO: issues or mrege_requests
GITLAB-NOTE-TO-IID:iid of gitlab-note-to
PAGE: current page number
PER-PAGE: number of items on page max 100
PARAMS: an alist for query parameters. Exple: '((state . \"opened\"))"
  (let ((params params)
        (notes)
        (x-total-pages
         (string-to-number
          (gitlab-get-header (s-concat "projects/"
                                       (url-hexify-string
                                        (format "%s" project-id))
                                       (format "/%s/" gitlab-note-to) (format "%s" gitlab-note-to-iid) "/notes"  )
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
          (setq notes (vconcat notes
                             (perform-gitlab-request "GET"
                                                     (s-concat "projects/"
                                                               (url-hexify-string
                                                                (format "%s" project-id))
                                                               (format "/%s/" gitlab-note-to) (format "%s" gitlab-note-to-iid) "/notes"
                                                               "?page=" (number-to-string page)
                                                               "&per_page=" (number-to-string per-page))
                                                     params
                                                     200)))
          (setq page (+ 1 page))
          while(<= page x-total-pages))
    notes))

(defun gitlab-show-notes (project-id gitlab-note-to gitlab-note-to-iid)
  "show notes of issue with a markdown buffer"
  (interactive)
  (let* ((notes (gitlab-list-notes project-id gitlab-note-to gitlab-note-to-iid 1 200))
         (project-name (assoc-default 'name (gitlab-get-project project-id))))
    (pop-to-buffer (format "*Gitlab Notes[%s/#%d]*" project-name gitlab-note-to-iid) nil)
    (gitlab-notes-mode)
    (page-break-lines-mode)
    (make-local-variable 'project-id)
    (make-local-variable 'gitlab-note-to)
    (make-local-variable 'gitlab-note-to-iid)
    (setq-local default-directory (projectile-project-root))
    (setq header-line-format
          (concat (propertize " " 'display '((space :align-to 0)))
                  "Return: create a new note"))
    (mapcar (lambda (note)
              (insert (format "id: %d\n=\n" (assoc-default 'id note)))
              (insert (format "Create by: %s\n" (assoc-default 'name (assoc-default 'author note))))
              ;; (insert (make-string 80 ?-))
              (insert "\n")
              (insert (assoc-default 'body note))
              (insert "\n")
              (insert "")
              (insert "\n")
              )
            notes)
    (read-only-mode 1)))

(defun gitlab-refresh-note ()
  "Refresh note content"
  (interactive)
  (read-only-mode -1)
  (erase-buffer)
  (gitlab-show-notes project-id gitlab-note-to gitlab-note-to-iid)
  )

(defun gitlab-show-issue-notes-current ()
  "show notes of current issue"
  (interactive)
  (let* ((project-id (assoc-default 'project_id (tabulated-list-get-id)))
         (issue-iid (assoc-default 'iid (tabulated-list-get-id))))
    (gitlab-show-notes project-id "issues" issue-iid)
    ))

(defun gitlab-show-mr-notes-current ()
  "show notes of current issue"
  (interactive)
  (let* ((project-id (assoc-default 'project_id (tabulated-list-get-id)))
         (mr-iid (assoc-default 'iid (tabulated-list-get-id))))
    (gitlab-show-notes project-id "merge_requests" mr-iid)
    ))

(defun gitlab-new-note ()
  "call `gitlab-edit-note'"
  (interactive)
  (gitlab-edit-note (list (cons 'project-id project-id)
                          (cons 'gitlab-note-to-iid gitlab-note-to-iid)
                          (cons 'gitlab-note-to gitlab-note-to)))
  )

(defun gitlab-edit-note (info)
  "Edit a note's body with markdown-mode, and return it

INFO: a list: ((project-id . N) (iid . N) (gitlab-note-to . STR))
PROJECT-ID: project id
IID: id of issue or MR?
GITLAB-NOTE-TO: this note is belong to a issues or merger_requests"

  (interactive)
  (pop-to-buffer (format "*Gitlab Edit Note*") nil)
  (gitlab-edit-note-mode)
  (let ()
    (make-local-variable 'project-id)
    (setq-local project-id (assoc-default 'project-id info))
    (make-local-variable 'gitlab-note-to-iid)
    (setq-local gitlab-note-to-iid (assoc-default 'gitlab-note-to-iid info))
    (make-local-variable 'gitlab-note-to)
    (setq-local gitlab-note-to (assoc-default 'gitlab-note-to info))
    )
  (setq header-line-format
        (concat (propertize " " 'display '((space :align-to 0)))
                (format "[%s] C-c C-c: finish Edit; Kill Buffer: Abort" (projectile-project-name))))
  (insert (format "SHA-1: %s\n\nFile: "
                  (magit-rev-parse "--short" "HEAD")))
  )

(defun gitlab-comment-new-note ()
  "do POST request"
  (interactive)
  (let* (;; (project-id project-id)
         ;; (iid iid)
         ;; (scope scope)
         (body (buffer-substring-no-properties (point-min) (point-max)))
         (params (list (cons 'body body))))
    (cond
     ((member gitlab-note-to '("issues" "merge_requests"))
       (progn (perform-gitlab-request "POST"
                                       (s-concat "projects/"
                                                 (url-hexify-string
                                                  (format "%s" project-id))
                                                 (format "/%s/" gitlab-note-to) (format "%s" gitlab-note-to-iid)
                                                 "/notes"
                                                 )
                                       params
                                       201 )
              (kill-buffer (buffer-name))))
      (t
        (user-error (format "undefined action to scope %s" gitlab-note-to)))))
  )

(provide 'my-gitlab-notes)
