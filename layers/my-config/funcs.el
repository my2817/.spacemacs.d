(defun wttr/prepend-to-exec-path (path)
  "prepand the path to the emacs intenral `exec-path' and \"PATH\" env variable.
    Return the updated `exec-path'"
  (setenv "PATH" (concat (expand-file-name path)
                         path-separator
                         (getenv "PATH")))
  (setq exec-path
        (cons (expand-file-name path)
              exec-path)))
(defadvice electric-spacing-. (around my-electric-spacing-.)
  "my advice function for electric-spacing-."
  (cond ((and electric-spacing-double-space-docs
              (electric-spacing-document?))
         (electric-spacing-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (string= major-mode "verilog-mode")
             (or (and c-buffer-is-cc-mode
                      (looking-back "[a-z]"))
                 (and
                  (derived-mode-p 'python-mode 'ruby-mode)
                  (looking-back "[a-z\)]"))
                 (and
                  (derived-mode-p 'js-mode 'js2-mode)
                  (looking-back "[a-z\)$]"))))
         (insert "."))
        ((derived-mode-p 'cperl-mode 'perl-mode 'ruby-mode)
         ;; Check for the .. range operator
         (if (looking-back ".")
             (insert ".")
           (insert " . ")))
        (t
         (electric-spacing-insert "." 'after)
         (insert " ")))
  )
(ad-activate 'electric-spacing-.)
(defadvice electric-spacing-insert-1 (around my-electric-spacing-insert-1)
  "save-excursion before do 'indent-according-to-mode"
  (pcase only-where
    (`before (insert " " op))
    (`middle (insert op))
    (`after (insert op " "))
    (_
     (let ((begin? (bolp)))
       (unless (or (looking-back (regexp-opt
                                  (mapcar 'char-to-string
                                          (mapcar 'car electric-spacing-rules)))
                                 (line-beginning-position))
                   begin?)
         (insert " "))
       (insert op " ")
       (when begin?
         (save-excursion
           (indent-according-to-mode))))))
  )
(ad-activate 'electric-spacing-insert-1)

(defun electric-spacing-verilog-mode-\( ()
  (delete-horizontal-space)
  (if (looking-back (regexp-opt (mapcar 'char-to-string
                                        "([{}])")))
      (electric-spacing-insert-1 "(" 'middle)
    (electric-spacing-insert-1 "(" 'before)))

(defadvice electric-spacing-? (around my-electric-spacing-?)
  "See `electric-spacing-insert'."
  (if (string= major-mode "verilog-mode")
      (electric-spacing-insert "?")
    (electric-spacing-insert "?" 'before )))
(ad-activate 'electric-spacing-?)

(defun find-file-in-path-list (file path-list )
  "find FILE in path-list, then return the absolute file path"
  (when path-list
    (if (file-exists-p (format "%s/%s" (car path-list) file))
        (format "%s/%s" (car path-list) file)
      (find-file-in-path-list file (cdr path-list)))))

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (-fpath)
           ;; (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
           (shell-command (concat "start " (replace-regexp-in-string "/" "\\" -fpath t t)))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (-fpath)
           (shell-command
            (concat "open " (shell-quote-argument -fpath))))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (-fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" -fpath))) -file-list))))))
(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    ;; (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
    (shell-command (concat "start " (replace-regexp-in-string "/" "\\" default-directory t t))))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. For example: with nautilus
    )))
(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    ;; (message "Microsoft Windows not supported. File a bug report or pull request."))
    (shell-command (concat "start")))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "gnome-terminal"
                     (concat "--working-directory=" default-directory)
                     "--profile=zsh") ))))

(defun my-config-error-regexp-add-emacs (regexp-alist)
  " add regexp-alist to `compilation-error-regexp-alist'
Called by `compilation-mode-hook'.  This allows \\[next-error] to
find the errors."
  (interactive)
  (mapcar
   (lambda (item)
     (if (not (memq (car item) compilation-error-regexp-alist))
         (progn
           (push (car item) compilation-error-regexp-alist)
           (push item compilation-error-regexp-alist-alist))
       )
     )
   regexp-alist))

(defun flycheck-verilog-run/create-catch-dir ()
  (if (and (projectile-project-p)
           (not (file-directory-p (concat (projectile-project-root) ".catch/irun/worklib")))
           )
      (progn
        (dired-create-directory (concat (projectile-project-root) ".catch/irun/worklib"))
        (message "fycheck-verilog/create-catch-dir : OK!")
        )
    (if (not (projectile-project-p))
        (message "Your don't inside a projecte!")
      (message "fycheck-verilog/create-catch-dir check: OK!")
      )
    ))

(defun my-project-dir-local-init()

  (interactive)
  (let* (
         (fn (concat (projectile-project-root) ".dir-locals.el")))
    (if (and (projectile-project-p)
             (not (file-exists-p fn))
             )
        (progn
          (find-file fn)
          (insert "
((verilog-mode . (
                  (eval .
                        (setq verilog-library-directories '(\".\"))
                        )
                  (eval .
                        (mapcar
                         (lambda (file)
                           (add-to-list 'verilog-library-directories (file-name-directory file)))
                         (directory-files-recursively
                          (concat (projectile-project-root) \"digital/rtl\") \"\\.[s]?v$\")
                         )
                        ))
               ))
")
          (save-buffer)
          (kill-buffer)
        )
      (message ".dir-locals exists: %s" fn)
      )
    )
  )

;; it will give a error like "arguments too long" if list all project file to the command line, so don't use the next adviced function
;; (defadvice projectile-regenerate-tags (around my-project-regenerate-tags)
;;   "we just generate tags of files which listed by `projectile-current-project-files'"
;;     (interactive)
;;     (if (and (boundp 'ggtags-mode)
;;              (memq projectile-tags-backend '(auto ggtags)))
;;         (progn
;;           (let* ((ggtags-project-root (projectile-project-root))
;;                  (default-directory ggtags-project-root))
;;             (ggtags-ensure-project)
;;             (ggtags-update-tags t)))
;;       (let* ((project-root (projectile-project-root))
;;              (tags-exclude (projectile-tags-exclude-patterns))
;;              (default-directory project-root)
;;              (tags-file (expand-file-name projectile-tags-file-name))
;;              (command (format projectile-tags-command tags-file tags-exclude default-directory))
;;              (current-project-files (mapconcat 'identity (projectile-current-project-files) " "))

;;              shell-output exit-code)
;;         ;; (setq command (format "%s %s" command current-project-files))
;;         (with-temp-buffer
;;           (setq exit-code
;;                 (call-process-shell-command command nil (current-buffer))
;;                 shell-output (string-trim
;;                               (buffer-substring (point-min) (point-max)))))
;;         (unless (zerop exit-code)
;;           (error shell-output))
;;         (visit-tags-table tags-file)
;;         (message "Regenerated %s" tags-file))))

(defadvice plantuml-indent-line (around my-plantuml-indent-line)
;; (defun plantuml-indent-line ()
  "Indent current line as plantuml code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      (let ((not-indented t) cur-indent var-indent)
        (if (looking-at plantuml-indent-regexp-end)
            (progn
              (save-excursion
                (forward-line -1)
                (if (looking-at plantuml-indent-regexp-start)
                    (setq cur-indent (current-indentation))
                  (setq cur-indent(- (current-indentation)
                                     plantuml-indent-offset))))
              (if (< cur-indent 0)
                  (setq cur-indent 0)))
          (save-excursion
            (while not-indented
              (forward-line -1)
              (cond
               ((looking-at plantuml-indent-regexp-start)
                (setq cur-indent (+ (current-indentation)
                                    plantuml-indent-offset)
                      not-indented nil))
               ((looking-at plantuml-indent-regexp-end)
                (setq cur-indent (current-indentation)
                      not-indented nil))
               ((progn (forward-line 1)
                       (setq var-indent
                             (looking-at plantuml-indent-regexp-arrow))
                       (forward-line -1)
                       var-indent)
                (cond
                 ((> (setq var-indent
                           (string-match
                            (progn (string-match
                                    plantuml-indent-regexp-arrow-1
                                    (current-line-string))
                                   (match-string-no-properties
                                    0
                                    (current-line-string)))
                            (current-line-string))) 0)
                  (setq cur-indent  var-indent
                        not-indented nil))))
               ((progn (forward-line 1)
                       (setq var-indent
                             (looking-at plantuml-indent-regexp-arrow-2))
                       (forward-line -1)
                       var-indent)
                (cond
                 ('t
                  (let ((var-count 0) (var-flag t))
                    (while var-flag
                      (incf var-count)
                      (forward-line -1)
                      (cond ((bobp) (setq var-flag nil))
                            ((looking-at plantuml-indent-regexp-arrow) nil)
                            ((looking-at "^\s+$") nil)
                            ((looking-at plantuml-indent-regexp-end) nil)
                            ((looking-at plantuml-indent-regexp-start) nil)
                            ('t (setq cur-indent (current-indentation)
                                      not-indented nil
                                      var-flag nil))))
                    (forward-line var-count)))))
               ((bobp) (setq not-indented nil))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0)))))
  (if (bolp)
      (end-of-line))
  )
(ad-activate 'plantuml-indent-line)


(defun org-projectile/update-agenda-files ()
  "Update org-agenda-files based on `org-projectile-todo-files'

if agenda file non-existent, DONT add is to org-agenda-files
"
  (interactive)
  (mapcar (lambda (f)
            (if (file-exists-p f)
                (add-to-list 'org-agenda-files f)))
          (org-projectile-todo-files)
          )
  )

(defun my-copy-file-line-to-clipboard()
  "copy current buffer name and line number to clipboard

and return as PATH-to-FILE::Line-Number."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (message "Copied: %s" (concat filename "::" (format "%s" (line-number-at-pos))))
      (kill-new (concat filename "::" (format "%s" (line-number-at-pos)))))))

(defadvice counsel-imenu (after my-counsel-imenu)
  "`evil-scroll-line-to-top' after `counsel-imenu'"
  (evil-scroll-line-to-center nil)
  )
(ad-activate 'counsel-imenu)


(defun my-bin2dec(&optional arg)
  (interactive "P")
  (let ((local-word (replace-regexp-in-string "[bB]" "" (thing-at-point 'word 'no-properties))))
    (message (format "bin2dex: %s -> %d" local-word (string-to-number  local-word 2))) )
  )

(defun my-bin2hex(&optional arg)
  (interactive "P")
  (let ((local-word (replace-regexp-in-string "[bB]" "" (thing-at-point 'word 'no-properties))))
    (message (format "bin2hex: %s -> %x" local-word (string-to-number  local-word 2))) )
  )

(defun my-dec2hex(&optional arg)
  "C-0 my-dec2hex to accepts user input"
  (interactive "P")
  (let ((local-word (if arg
                        (ivy-completing-read "input dec: " nil)
                      (replace-regexp-in-string "[dD]" "" (thing-at-point 'word 'no-properties)))))
    (message (format "dex2hex : %s -> %x" local-word (string-to-number  local-word 10))) )
  )

(defun my-hex2dec(&optional arg)
  "C-0 my-hex2dec to accepts user input"
  (interactive "P")
  (let ((local-word (if arg
                        (ivy-completing-read "input hex: " nil)
                      (replace-regexp-in-string "[xXhH]" "" (thing-at-point 'word 'no-properties)))))
    (message (format "dex2hex : %s -> %d" local-word (string-to-number  local-word 16))) )
  )

(defun my-yank-image-from-win-clipboard-through-powershell()
  "to simplify the logic, use c:/Users/Public as temporary directoy, the move it into current directoy

Anyway, if need to modify the file name, please DONT delete or modify file extension \".png\",
otherwise you will this function don't work and don't know how
"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (file-name (format "%s" (read-from-minibuffer "Img Name:" (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))))
         ;; (file-path-powershell (concat "c:/Users/\$env:USERNAME/" file-name))
         (file-path-wsl (concat "./images/" file-name)))
    (if (file-exists-p "./images")
        (ignore)
      (make-directory "./images"))
    ;; (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/\\$env:USERNAME/" file-name "\\\")\""))
    (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/Public/" file-name "\\\")\""))
    (rename-file (concat "/mnt/c/Users/Public/" file-name) file-path-wsl)
    (format "%s" file-path-wsl)
    ))

(defun my-yank-image-link-into-org-from-wsl ()
  "call `my-yank-image-from-win-clipboard-through-powershell' and insert image file link with org-mode format"
  (interactive)
  (let* ((file-path (my-yank-image-from-win-clipboard-through-powershell))
         (file-link (format "[[file:%s][%s]]" file-path (file-name-sans-extension (file-name-nondirectory file-path))))
         )
    (insert file-link)
    ))

(defun xah-open-in-desktop-from-wsl()
  "open desktop by send command from wsl into powershell"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (directory (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" default-directory))
         )
    (shell-command (concat powershell " -command \"start " directory "\""))
    )
  )

(defun xah-open-in-xternal-app-from-wsl()
  "open desktop by send command from wsl into powershell"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (directory (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" default-directory))
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         )
    (shell-command (concat powershell " -command \"start " (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" (nth 0 -file-list)) "\""))
    )
  )

;; The free version of TabNine is good enough,
;; and below code is recommended that TabNine not always
;; prompt me to purchase a paid version in a large project.
(defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
  (let ((company-message-func (ad-get-arg 0)))
    (when (and company-message-func
               (stringp (funcall company-message-func)))
      (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
        ad-do-it))))

(defun my-ediff-dired-marked-files ()
  "call `ediff-files' with args return by `dired-get-marked-files'"
  (interactive)
  (let* ((files (dired-get-marked-files))
         (fileA (nth 0 files))
         (fileB (nth 1 files))
         (fileC (if (>= (length files) 3)
                    (nth 2 files))))
    (if (= (length files) 2)
        (ediff-files fileA fileB)
      (ediff-files3 fileA fileB fileC))))

(defun ora-ediff-files ()
  "
    In case no files are marked, the file at point is used as the first file, and read-file-name is used for the second file. Since I have the magic (setq dired-dwim-target t) in my config, in case a second dired buffer is open, dired-dwim-target-directory will offer it as the starting directory during completion. Very useful to compare two files in two different directories.

    Depending on the order of the arguments to ediff-files, the changes will appear either as added or removed; file-newer-than-file-p tries to put the arguments in a logical order by looking at the files' last change times.

    ediff-after-quit-hook-internal is used to restore the previous window configuration after I quit ediff with q.
"
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file: "
                        (dired-dwim-target-directory)))))
          (if (file-newer-than-file-p file1 file2)
              (ediff-files file2 file1)
            (ediff-files file1 file2))
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (setq ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (let* ((file1 (nth 0 files))
             (file2 (nth 1 files))
             (file3 (nth 2 files)))
        (ediff-files3 file1 file2 file3)
        (add-hook 'ediff-after-quit-hook-internal
                  (lambda ()
                    (setq ediff-after-quit-hook-internal nil)
                    (set-window-configuration wnd)))))))

(defun my-highlight-symbol-in-frame ()
  "highlight symbol in current frame

`window-list'
`select-window'
"
  (interactive)
  (let* ((symbol (highlight-symbol-get-symbol))
         (window-list (window-list))
         (window-cur (nth 0 window-list))
         )
    (save-excursion
      (dolist (window window-list)
        (select-window window)
        (if (highlight-symbol-symbol-highlighted-p symbol )
            (highlight-symbol-remove-symbol symbol)
          (highlight-symbol-add-symbol symbol "yellow")
          (when (member 'explicit highlight-symbol-occurrence-message)
            (highlight-symbol-count symbol t)))
        )
      (select-window window-cur))))

;; bsub as proxy of git to rd3
(defun my-git-fetch-through-by-bsub ()
  "send \"git fetch --all\" to bsub "
  (interactive)
  (shell-command (concat "bsub -R type=any -Is \'git fetch --all\'"))
  (magit-refresh)
  )

;; (spacemacs|add-toggle git-on-bsub
;;   :status (not (string= magit-git-executable "git"))
;;   :on (custom-set-variables '(magit-git-executable "bsub -m rd3 git") )
;;   :off (custom-set-variables '(magit-git-executable "git") )
;;   :documentation "Toggle use bsub as proxy for git or not"
;;   ;;:evil-leader "tD"
;; )

(with-eval-after-load 'magit-fetch
  (define-transient-command magit-fetch ()
    "Fetch from another repository."
    :man-page "git-fetch"
    ["Arguments"
     ("-p" "Prune deleted branches" ("-p" "--prune"))
     ("-t" "Fetch all tags" ("-t" "--tags"))]
    ["Fetch from"
     ("p" magit-fetch-from-pushremote)
     ("u" magit-fetch-from-upstream)
     ("e" "elsewhere"        magit-fetch-other)
     ("a" "all remotes"      magit-fetch-all)
     ("b" "all remotes through bsub"       my-git-fetch-through-by-bsub)]
    ["Fetch"
     ("o" "another branch"   magit-fetch-branch)
     ("r" "explicit refspec" magit-fetch-refspec)
     ("m" "submodules"       magit-fetch-modules)
     ]
    ["Configure"
     ("C" "variables..." magit-branch-configure)])
  )

(with-eval-after-load 'magit-push
  (define-suffix-command bsub-magit-push-current-to-pushremote (args)
    "Push the current branch to its push-remote.

When the push-remote is not configured, then read the push-remote
from the user, set it, and then push to it.  With a prefix
argument the push-remote can be changed before pushed to it."
    :if 'magit-get-current-branch
    :description 'magit-push--pushbranch-description
    (interactive (list (magit-push-arguments)))
    (pcase-let ((`(,branch ,remote)
                 (magit--select-push-remote "push there")))
      (run-hooks 'magit-credential-hook)
      (shell-command (concat "bsub -R type=any -Is \'git " "push " "-v " args " " remote " "
                             (format "refs/heads/%s:refs/heads/%s\'"
                                     branch branch)))
      (magit-refresh)
      ))

  (define-transient-command magit-push ()
    "Push to another repository."
    :man-page "git-push"
    ["Arguments"
     ("-f" "Force with lease" (nil "--force-with-lease"))
     ("-F" "Force"            ("-f" "--force"))
     ("-h" "Disable hooks"    "--no-verify")
     ("-n" "Dry run"          ("-n" "--dry-run"))
     (5 "-u" "Set upstream"   "--set-upstream")
     (7 "-t" "Follow tags"    "--follow-tags")]
    [:if magit-get-current-branch
         :description (lambda ()
                        (format (propertize "Push %s to" 'face 'transient-heading)
                                (propertize (magit-get-current-branch)
                                            'face 'magit-branch-local)))
         ("p" magit-push-current-to-pushremote)
         ("u" magit-push-current-to-upstream)
         ("e" "elsewhere" magit-push-current)
         ("b" "bsub push" bsub-magit-push-current-to-pushremote)
         ]
    ["Push"
     [("o" "another branch"    magit-push-other)
      ("r" "explicit refspecs" magit-push-refspecs)
      ("m" "matching branches" magit-push-matching)]
     [("T" "a tag"             magit-push-tag)
      ("t" "all tags"          magit-push-tags)]]
    ["Configure"
     ("C" "Set variables..."  magit-branch-configure)])
  )
