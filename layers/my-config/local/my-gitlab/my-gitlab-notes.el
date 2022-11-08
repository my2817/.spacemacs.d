(require 'gitlab)
(require 'markdown-mode)

(define-key gitlab-issues-mode-map (kbd "RET") 'gitlab-show-issue-notes-current)

(defvar gitlab-notes-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    ;; (define-key map (kbd "v") 'print-current-issue-id)
    ;; (define-key map (kbd "w") 'gitlab-goto-issue)
    (define-key map (kbd "RET") 'gitlab-issue-new-note)
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

(defun gitlab-list-issue-notes (project-id issue-iid &optional page per-page params)
  "Get a list of issue notes
PROJECT-ID : The ID of a project
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
                                       "/issues/" (format "%s" issue-iid) "/notes"  )
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
                                                               "/issues/" (format "%s" issue-iid) "/notes"
                                                               "?page=" (number-to-string page)
                                                               "&per_page=" (number-to-string per-page))
                                                     params
                                                     200)))
          (setq page (+ 1 page))
          while(<= page x-total-pages))
    notes))

(defun gitlab-show-issue-notes (project-id issue-iid)
  "show notes of issue with a markdown buffer"
  (interactive)
  (let* ((notes (gitlab-list-issue-notes project-id issue-iid 1 200))
         (project-name (assoc-default 'name (gitlab-get-project project-id))))
    (pop-to-buffer (format "*Gitlab Notes[%s/#%d]*" project-name issue-iid) nil)
    (gitlab-notes-mode)
    (page-break-lines-mode)
    (make-local-variable 'project-id)
    (make-local-variable 'issue-iid)
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

(defun gitlab-show-issue-notes-current ()
  "show notes of current issue"
  (interactive)
  (let* ((project-id (assoc-default 'project_id (tabulated-list-get-id)))
         (issue-iid (assoc-default 'iid (tabulated-list-get-id))))
    (gitlab-show-issue-notes project-id issue-iid)
    ))

(defun gitlab-issue-new-note ()
  "call `gitlab-edit-note'"
  (interactive)
  (gitlab-edit-note (list (cons 'project-id project-id)
                          (cons 'iid issue-iid)
                          (cons 'scope "issue")))
  )

(defun gitlab-edit-note (info)
  "Edit a note's body with markdown-mode, and return it

INFO: a list: ((project-id . N) (iid . N) (scope . STR))
PROJECT-ID: project id
IID: id of issue or MR?
SCOPE: this note is belong to a issue or MR"

  (interactive)
  (pop-to-buffer (format "*Gitlab Edit Note*") nil)
  (gitlab-edit-note-mode)
  (let ()
    (make-local-variable 'project-id)
    (setq-local project-id (assoc-default 'project-id info))
    (make-local-variable 'iid)
    (setq-local iid (assoc-default 'iid info))
    (make-local-variable 'scope)
    (setq-local scope (assoc-default 'scope info))
    )
  (setq header-line-format
        (concat (propertize " " 'display '((space :align-to 0)))
                "C-c C-c to comment this"))
  )

(defun gitlab-comment-new-note ()
  "do POST request"
  (interactive)
  (let* (;; (project-id project-id)
         ;; (iid iid)
         ;; (scope scope)
         (body (buffer-substring-no-properties (point-min) (point-max)))
         (params (list (cons 'body body))))
    (pcase scope
      ("issue"
       (progn (perform-gitlab-request "POST"
                                       (s-concat "projects/"
                                                 (url-hexify-string
                                                  (format "%s" project-id))
                                                 "/issues/" (format "%s" iid)
                                                 "/notes"
                                                 )
                                       params
                                       201 )
              (kill-buffer (buffer-name))))
      (_
        (user-error (format "undefined action to scope %s" scope)))))
  )

(provide 'my-gitlab-notes)
