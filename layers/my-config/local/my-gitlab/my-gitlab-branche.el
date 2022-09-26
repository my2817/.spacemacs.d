(require 'gitlab)
(defun gitlab-list-project-branches-name (branches)
  "return a list of branches name"
  (mapcar (lambda (b)
            (assoc-default 'name b))
          branches)
  )

(defun gitlab-list-project-branches (project-id &optional page per-page params)
  "Get a list of project branches.
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
                                      "/repository/branches")
                            params
                            200)))

;;;###autoload
(provide 'my-gitlab-branche)
