;;;  -*- lexical-binding: t -*-
(defvar citre-auto-update-process-table
  #s(hash-table
     test equal
     )
  "a hash table to store sub-process id of every project,
which key if `(funcall citre-project-root-function)'")

(defvar citre-auto-update-after-process-table
  #s(hash-table
     test equal
     )
  "a hash table to store sub-process name of after-process(edit ptags),
which key if `(funcall citre-project-root-function)'.
If a after-process exist, wait, don't interrupt it.")

(defvar citre-auto-update-ptag-cache
  #s(hash-table
     test equal
     )
  "a hash table to as cached pesudo of citre, which used to write back to *tagsfile.auto-update
key: project-root
vlaue: '((CITRE_CMD code) (TAG_PROC_CWD value))
")
(defvar citre-auto-update-timer-cache
  #s(hash-table
     test equal
     )
  "a hash table to as cached pesudo of citre, which used to write back to *tagsfile.auto-update
key: project-root
vlaue: timer object
")

(defvar citre-auto-update-timer nil
  "timer to run `citre-auto-update'")

(defun citre-auto-update-run-timer ()
  ;; TODO: if the timer is very long to decrease the run frequence of ctags,
  ;; maybe we can switch to other project, we need give every project a uniquified timer
  ;; record cwd and tagsfile, so that the timer can be a long time, and we can switch to other project
  (let* ((cwd (expand-file-name (funcall citre-project-root-function)))
         (tagsfile (citre-tags-file-path))
         (timer (gethash cwd citre-auto-update-timer-cache)))
    (when timer
      (cancel-timer timer))
    ;; if a after-process is running, don't interrupt it.
    (or (gethash cwd citre-auto-update-after-process-table)
       (setq timer
             (run-with-idle-timer 20 nil
                                  'citre-auto-update cwd tagsfile)))
    (puthash cwd timer citre-auto-update-timer-cache)))


(defvar citre-auto-update--root (file-name-directory (or load-file-name buffer-file-name))
  "The path to the root of citre-auto-update.
This path is location to find citre-auto-update-after-process.sh.
*-after-process.sh is a shell script to insert CITRE_CMD and TAG_PROC_CWD in temp file
so that don't block emacs.
")

(defun citre-auto-update-template-tags ()
  "return tags file path with sufix \".auto-update\""
  (let* ((tags-file-name (citre-tags-file-path)))
    (concat tags-file-name ".auto-update")))

(defun citre-auto-update-enable ()
  "add `citre-auto-update' to `after-save-hook'"
  (interactive)
  (add-hook 'after-save-hook 'citre-auto-update-run-timer)
  (clrhash citre-auto-update-ptag-cache)
  )

(defun citre-auto-update-disable ()
  "remove `citre-auto-update' to `after-save-hook'"
  (interactive)
  (remove-hook 'after-save-hook 'citre-auto-update-run-timer)
  (clrhash citre-auto-update-ptag-cache)
  )

;;;###autoload
(defun citre-auto-update(&optional cwd tagsfile)
  "auto update citre tagsfile of current project if `citre-mode' is non-nil
"
  (when citre-mode
    (let* (;;(default-directory cwd)
           (tagsfile (or tagsfile (citre-tags-file-path)))
           (tagsfile-tmp (concat tagsfile ".auto-update"))
           (project-root (or cwd (expand-file-name (funcall citre-project-root-function))))
           (runing-proc (gethash project-root citre-auto-update-process-table))
           (runing-after-proc (gethash project-root citre-auto-update-after-process-table)) )
      ;; kill runing process
      (when runing-after-proc
        (and (get-process runing-after-proc)
             (message "kill runing after-process: %s" runing-after-proc)
             (signal-process (get-process runing-after-proc) 'stop))
        (remhash project-root citre-auto-update-after-process-table))

      (when runing-proc
        (and (get-process runing-proc)
             (message "kill runing process: %s" project-root)
             (signal-process (get-process runing-proc) 'stop))
        (and (file-exists-p tagsfile-tmp)
             (delete-file tagsfile-tmp))
        (remhash project-root citre-auto-update-process-table))

      ;; generate new tagsfile-tmp
      (copy-file tagsfile tagsfile-tmp t)
      ;; (sit-for 0.001 )
      (citre-auto-update-updatable-tags-file tagsfile-tmp nil cwd))))

(defun citre-auto-update-updatable-tags-file (&optional tagsfile sync cwd)
  " copy from `citre-upate-updateale-tas-file', to generate tagsfile-tmp.

Update TAGSFILE that contains recipe for updating itself.
If the recipe can't be found, throw an error.

When SYNC is non-nil, update TAGSFILE synchronously.

Return t if the ctags process starts successfully (when updating
asynchronously), or the updating is finished (when updating
synchronously).  Otherwise return nil."
  (when-let* ((tagsfile
               (or tagsfile (citre-read-tags-file-name)))
              (cmd-ptag (citre-auto-update-get-ptag-from-cache cwd "CITRE_CMD"))
              (cmd (citre--ctags-cmd-ptag-to-exec cmd-ptag tagsfile))
              (cwd-ptag (citre-auto-update-get-ptag-from-cache cwd "TAG_PROC_CWD"))
              (cwd (if-let ((remote-id (file-remote-p tagsfile)))
                       (concat remote-id cwd-ptag) cwd-ptag))
              (proc-name (concat tagsfile "." "proc"))
              (after-process  (lambda ()
                                (let* (
                                       ;; TODO don't read pseduo tag by citre, cache it.
                                       ;; Q: when to updatge the cached psdudo
                                       ;;    A: a advice code after citre-update-this-tags-file
                                       ;; (cmd-ptag (citre--get-pseudo-tag-value "CITRE_CMD" (string-replace ".auto-update" "" tagsfile )))
                                       ;; (cwd-ptag (citre--get-pseudo-tag-value "TAG_PROC_CWD" (string-replace ".auto-update" "" tagsfile )))
                                       )
                                  ;; (citre--write-ctags-recipe
                                  ;;  tagsfile cmd-ptag cwd-ptag)
                                  ;;this shell scripte getCITRE_CMD and TAG_PROC_CWD from old tagsfile
                                  ;; (start-process (concat tagsfile ".after-proc") "*citre-ctags*"
                                  ;;                "sh" (concat citre-auto-update--root "citre-auto-update-after-process.sh") tagsfile)
                                  (remhash (or cwd (expand-file-name (funcall citre-project-root-function))) citre-auto-update-process-table)

                                  (puthash (or cwd (expand-file-name (funcall citre-project-root-function)))
                                           (concat tagsfile ".after-proc")
                                           citre-auto-update-after-process-table)
                                  (let ((default-directory cwd))
                                    (make-process
                                     :name (concat tagsfile ".after-proc")
                                     :buffer (get-buffer-create "*citre-ctags*")
                                     :command (list "sh" (concat citre-auto-update--root "citre-auto-update-after-process.sh")
                                                    tagsfile (citre-auto-update-get-ptag-from-cache cwd "CITRE_CMD")
                                                    (citre-auto-update-get-ptag-from-cache cwd "TAG_PROC_CWD") )
                                     :connection-type 'pipe
                                     :stderr nil
                                     :sentinel (lambda (proc _msg)
                                                 (remhash (or cwd (expand-file-name (funcall citre-project-root-function))) citre-auto-update-after-process-table)
                                                 ;; (message "Finished auto-updating %s" tagsfile)
                                                 (citre-clear-tags-file-cache))))))))
    ;; Workaround: If we put this let into the above `if-let*' spec, even
    ;; if it stops before let-binding `default-directory', later there'll
    ;; be some timer errors.
    (puthash (or cwd (expand-file-name (funcall citre-project-root-function))) proc-name citre-auto-update-process-table)
    (let ((default-directory cwd))
      (if sync
          (progn (apply #'process-file (car cmd) nil
                        (get-buffer-create "*citre-ctags*") nil (cdr cmd))
                 (funcall after-process))
        (make-process
         :name proc-name
         :buffer (get-buffer-create "*citre-ctags*")
         :command cmd
         :connection-type 'pipe
         :stderr nil
         :sentinel
         (lambda (proc _msg)
           (pcase (process-status proc)
             ('exit
              (pcase (process-exit-status proc)
                (0 (funcall after-process)
                   ;; (message "Finished auto-updating %s" tagsfile)
                   )
                (s (user-error "Ctags exits %s.  See *citre-ctags* buffer"
                               s))))
             (s (user-error "Abnormal status of ctags: %s.  \
See *citre-ctags* buffer" s))))
         :file-handler t)
        ;; (message "Auto-updating %s..." tagsfile)
        )
      t)))

(defun citre-auto-update-get-ptag-from-cache (cwd ptag)
  "return ptag's value from `citre-auto-update-ptag-cache';
or update cache if value is nil"
  (let* ((project-root (or cwd (expand-file-name (funcall citre-project-root-function))))
         (pvalue  (assoc-default ptag (gethash project-root citre-auto-update-ptag-cache))))
    (or pvalue (message "run `citre-auto-update-set-ptag-to-cache' to update ptag-cache"))
    pvalue))

(defun citre-auto-update-set-ptag-to-cache ()
  "read CITRE_CMD and TAG_PROC_CWD from tagsfile, set to `citre-auto-update-ptag-cache'.
run after `citre-update-this-tags-file'
"
  (interactive)
  (sit-for 0.001)
  (let* ((tagsfile (citre-tags-file-path))
         (cmd-ptag (citre--get-pseudo-tag-value "CITRE_CMD" tagsfile))
         (cwd-ptag (citre--get-pseudo-tag-value "TAG_PROC_CWD" tagsfile))
         (project-root (expand-file-name (funcall citre-project-root-function))))
    (puthash project-root
             (list (cons "CITRE_CMD" cmd-ptag) (cons "TAG_PROC_CWD" cwd-ptag))
             citre-auto-update-ptag-cache)))


;; (defun citre-auto-update-process-after-process ()
;;   "For every project, key of `citre-auto-update-process-table', if it's sub-process exited,
;; goto that project and do something.

;; don't need this function, because `lexical-binding:t' can fix the problem of: void variables of after-processes
;; "
;;   (maphash (lambda (project proc-name)
;;              (let* ((proc-status (when-let ((proc (get-process proc-name)))
;;                                    (process-status proc))))
;;                (pcase proc-status
;;                  ('nil
;;                   (let* (
;;                          (default-directory project)
;;                          (tagsfile (citre-tags-file-path))
;;                          (cmd-ptag (citre--get-pseudo-tag-value "CITRE_CMD" tagsfile))
;;                          (tagsfile (concat tagsfile ".auto-update"))
;;                          (cwd-ptag (citre--get-pseudo-tag-value "TAG_PROC_CWD" tagsfile))
;;                          )
;;                     (citre--write-ctags-recipe
;;                      tagsfile cmd-ptag cwd-ptag)
;;                     (sit-for 1)
;;                     (rename-file tagsfile (string-replace ".auto-update" "" tagsfile ) t)
;;                     (citre-clear-tags-file-cache)
;;                     (remhash (funcall citre-project-root-function) citre-auto-update-process-table)
;;                     (message "after-process default-directory: %s" default-directory)
;;                     )
;;                   ))))
;;            citre-auto-update-process-table)
;;   )

(provide 'citre-auto-update)
