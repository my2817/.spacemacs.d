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

(provide 'my-gitlab)
