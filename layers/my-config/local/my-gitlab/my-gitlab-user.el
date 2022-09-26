(require 'gitlab)

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

(defun gitlab-get-user-id-by-username (username)
  "go through all pags to find user name"
  (let* ((per-page 100)
         (page 1)
         (x-total-pages (string-to-number (gitlab-get-header "users" "X-Total-Pages" 200 1 per-page)))
         (user (gitlab-get-user-id (gitlab-list-users page per-page) username))
         )
    (catch 'username
      (loop do
            (if user
                (throw 'username user)
              (setq page (1+ page))
              (setq user (gitlab-get-user-id (gitlab-list-users page per-page) username))
              )
            while (<= page x-total-pages)))
    (if (not user)
        (error "Can't find username in gitlab: %s" username )
      user)))

(defun gitlab-get-user-id (users username)
  "Return users->name"
  (let (id)
    (mapcar (lambda (user)
              (if (string= username (assoc-default 'username user))
                  (setq id (assoc-default 'id user))))
            users)
    id
    ))

;;;###autoload
(provide 'my-gitlab-user)
