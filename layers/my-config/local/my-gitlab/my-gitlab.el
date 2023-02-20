(require 'gitlab)
(require 'my-gitlab-user)
(require 'my-gitlab-branche)
(require 'my-gitlab-mr)
(require 'my-gitlab-notes)
(require 'my-gitlab-issues)

(defvar-local gitlab-project nil
  "gitlab project info")

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

(define-key gitlab-projects-mode-map (kbd "?") 'my-gitlab-projects-menu/body)

(defhydra my-gitlab-projects-menu (:hint none)
  "
^Actions^
^^^^^^^^----------------------------------------------------------
_v_: project ID
_w_: Open in web
_d_: Description
_m_: Mrs
_i_: Issues
"
  ("v" print-current-project-id )
  ("w" gitlab-goto-project )
  ("d" gitlab-describe-project )
  ("m" gitlab-mr-for-project)
  ("i" gitlab-issues-for-project)
  )

(provide 'my-gitlab)
