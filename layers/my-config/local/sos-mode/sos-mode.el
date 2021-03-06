(defgroup sos-mode nil
  "sos-mode group")
(defcustom sos-get-log-command 'my-verilog-get-last-history-log
  "`sos-op-on-file' will call this function to get the initial log for SOS CI command"
  :group 'sos-mode)
(defcustom sos-cmd '( "addreference" "audit" "ci" "co" "create" "definebranch"
"definetag" "delete" "deleterev" "deleteworkarea" "diff" "dirrev" "discardco"
"displaytmp" "exitsos" "exportrev" "expand" "gui" "help" "history" "merge"
"modattr" "move" "neverpopulate" "newworkarea" "nobjstatus" "nogui" "objstatus"
"pack" "populate" "preference" "print" "query" "objrso" "rename" "retirebranch"
"retiresnapshot" "retiretag" "revertrev" "select" "shell" "snapshot" "status"
"tag" "termbranch" "undelete" "unpopulate" "unselect" "update" "updatesel"
"userev" )
  "soscmd commands"
  :group 'sos-mode)
(defcustom sos-select-option '( "-A" "-sfile<file>" "-sco" "-suco" "-scm"
"-scnm" "-sncm" "-sne" "-slk" "-snt" "-snm" "-snl" "-sr" "-sname<pattern>"
"-sNr" "-sNh" "-sunm" "-sunmp" "-sunp" "-sp" "-sm" "-sall" "-sfo" "-sdo" "-sw"
"-sb" "-sxup" "-slbl<label>" "-satr<name>" "-satr<name=value>" "-si" "-sor"
"-sand" "-sxr" "-sproj<proj name>" "-yscroll")
  "option of select command"
  :group 'sos-mode
  )

(defcustom sos-attribute '( "CheckInTime" "CheckInTime" "CheckOutTime"
"CheckedInBy" "CheckedOutBy" "Checksum" "Description" "Group" "Log"
"MatchedLabel" "Owner" "PackageList" "PackageTypeList" "ReadAccess" "Reference"
"Revision" "SOS_RCPgm" "Trigger" "Version" "Writable" "WriteAccess"
"change_summary" "chkout_path" )
  "sos attributs"
  :group 'sos-mode)
(defcustom sos-attribute-value '(
                                 "user"
                                 "group"
                                 "world"
                                 )
  "sos attributte-values"
  :group 'sos-mode)
(defcustom file-list ""
  "sos file-list"
  :group 'sos-mode)
(defun sos-get-files ()
  (setq file-list "")
  (if (string-equal major-mode "dired-mode")
      (mapcar
       (lambda (file)
         (setq file-list (concat file-list " " file)))
       (dired-get-marked-files))
    (setq file-list (buffer-file-name))))

(defun sos-op-on-file (&optional arg)
  "exceute soscmd on current file or the selected files in dired-mode
where ARG is gived by c-0
"
  (interactive "P")
  (setq soscmd "soscmd")
  (sos-get-files)
  (cond
   (ivy-mode (setq sos-comp-read 'ivy-completing-read))
   (helm-mode (setq sos-comp-read 'helm-comp-read))
   (t (error "no ivy or helm cmplete read function is enabled"))
   )

  ;; (setq op (read-string "SOS actions: "))
  (setq op (funcall sos-comp-read "SOS actions: " sos-cmd))
  (setq soscmd (concat soscmd " "
                       op ))
  (setq soscmd (concat soscmd " "
                       file-list))
  (pcase op
    ("co" (progn
            (setq soscmd (concat soscmd " -Novr"))
            ;; (shell-command soscmd)
            ))
    ("ci" (progn
            (setq soscmd (concat soscmd " "
                                 "-aLog=\""
                                 (read-string "CI Log:" (funcall sos-get-log-command))
                                 "\""))
            ;; (shell-command soscmd)
            ))
    ("modattr" (progn
                 (setq soscmd (concat soscmd " "
                                      "-a"
                                      (funcall sos-comp-read "Arttribute: " sos-attribute) "="
                                      (funcall sos-comp-read "Arttribute\'s value: " sos-attribute-value)
                                      ))
                 ;; (shell-command soscmd)
                 ))
    ("select" (progn
                (setq soscmd (concat soscmd " "
                                     (funcall sos-comp-read "Options: " sos-select-option)))
                ;; (shell-command soscmd)
                ))
    ("diff" (progn
              (setq soscmd (concat soscmd " -gui"))
              (if arg
                  (cond ((zerop (prefix-numeric-value arg))
                         (setq soscmd (concat soscmd " " file-list "/" (funcall sos-comp-read "diff VS (REVISION/Branch): " nil))))))
              ))
    ("history" (progn
                 (setq soscmd (concat soscmd " -fs"))
                 ;; (shell-command soscmd)
                 ))
    ("userev" (progn
                (setq soscmd (concat soscmd "/" (funcall sos-comp-read "Revision to:" nil)))
                 ;; (shell-command soscmd)
                 ))
    ;; (_ (shell-command soscmd))
    )
  (if arg
      (cond ((zerop (prefix-numeric-value arg))
             (setq soscmd (concat soscmd " " (funcall sos-comp-read "Others Options: " nil))))))

  (shell-command soscmd)
  ;; need to revert buffer?
  (pcase op
    ( (or "co" "discardco" "ci" "userev") (progn
                              (revert-buffer t t )
                              )))
  ;; (message soscmd)
  )

;;;###autoload
(define-minor-mode sos-mode
  "commands used in the version control system SOS"
  :group 'sos-mode
  :init-value nil
  ;; :lighter "SOS"
  :global t
  :after-hook nil

  )
