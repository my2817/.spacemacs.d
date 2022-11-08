(require 'gitlab)
(require 'my-gitlab-user)
(require 'my-gitlab-branche)
(require 'my-gitlab-mr)
(require 'my-gitlab-notes)

(defvar-local gitlab-project nil
  "gitlab project info")
(define-key gitlab-projects-mode-map (kbd "m") 'gitlab-mr-for-project)

(defun gitlab-mr-for-project (&optional project-id)
  "From projects buffer, opens MRs buffer for project at point."
  (interactive)
  (let* ((project (gitlab-get-project (or project-id (tabulated-list-get-id))))
         (project-name (assoc-default 'name project))
         (state (ivy-completing-read "MR state:" '(opened closed merged))))
    (if project
	(progn
	  (pop-to-buffer (format "*Gitlab MRs[%s]*" project-name) nil)
	  (gitlab-mrs-mode)
          (setq-local gitlab-project project)
	  (setq tabulated-list-entries
		(create-mr-entries (gitlab-list-project-mrs (assoc-default 'id project) 1 100 (list (cons "state" state)))))
	  (tabulated-list-print t)
	  (tabulated-list-sort 1)
          )
      (user-error "No project here"))))

(defvar gitlab-mrs-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C") 'gitlab-close-mr)
    (define-key map (kbd "c") 'gitlab-create-mr)
    (define-key map (kbd "r") 'gitlab-reopen-mr)
    (define-key map (kbd "m") 'gitlab-merge-mr)
    (define-key map (kbd "R") 'gitlab-refresh-mr-list)
    (define-key map (kbd "u") 'gitlab-update-mr)
    map)
  "Keymap for `gitlab-mrs-mode' major mode.")

;; (define-key gitlab-issues-mode-map (kbd "c") 'gitlab-close-issue)
;; (define-key gitlab-issues-mode-map (kbd "o") 'gitlab-open-issue)



(define-derived-mode gitlab-mrs-mode tabulated-list-mode "Gitlab MRs"
  "Major mode for browsing Gitlab MRs."
  :group 'gitlab
  (setq tabulated-list-format [;("ID" 5 t)
                               ("State" 10 t)
                               ("Merge_status" 20)
                               ("Source" 15)
                               ("Target" 15)
                               ;; ("Project" 15 t) ;; performance, don't use this
                               ("Author" 15 t)
                               ("Assignee" 10 t)
                               ("Title" 0 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Title" nil))
  (tabulated-list-init-header))

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
