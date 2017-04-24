(defgroup sos-mode nil
  "sos-mode group")
(defcustom sos-cmd '(
               "addreference"
               "audit"
               "ci"
               "co"
               "create"
               "definebranch"
               "definetag"
               "delete"
               "deleterev"
               "deleteworkarea"
               "diff"
               "dirrev"
               "discardco"
               "displaytmp"
               "exitsos"
               "exportrev"
               "expand"
               "gui"
               "help"
               "history"
               "merge"
               "modattr"
               "move"
               "neverpopulate"
               "newworkarea"
               "nobjstatus"
               "nogui"
               "objstatus"
               "pack"
               "populate"
               "preference"
               "print"
               "query"
               "objrso"
               "rename"
               "retirebranch"
               "retiresnapshot"
               "retiretag"
               "revertrev"
               "select"
               "shell"
               "snapshot"
               "status"
               "tag"
               "termbranch"
               "undelete"
               "unpopulate"
               "unselect"
               "update"
               "updatesel"
               "userev"
               )
  "soscmd commands"
  :group 'sos-mode)

(defcustom sos-attribute '(
                      "CheckInTime"
                      "CheckInTime"
                      "CheckOutTime"
                      "CheckedInBy"
                      "CheckedOutBy"
                      "Checksum"
                      "Description"
                      "Group"
                      "Log"
                      "MatchedLabel"
                      "Owner"
                      "PackageList"
                      "PackageTypeList"
                      "ReadAccess"
                      "Reference"
                      "Revision"
                      "SOS_RCPgm"
                      "Trigger"
                      "Version"
                      "Writable"
                      "WriteAccess"
                      "change_summary"
                      "chkout_path"
                      )
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
  (mapcar
   (lambda (file)
     (setq file-list (concat file-list "" file)))
   (if (string-equal major-mode "dired-mode")
       (dired-get-marked-files)
     (list (buffer-file-name)))))

(defun sos-op-on-file ()
  "exceute soscmd on current file or the selected files in dired-mode"
  (interactive)
  (setq soscmd "soscmd")
  (sos-get-files)
  ;; (setq op (read-string "SOS actions: "))
  (setq op (helm-comp-read "SOS actions: " sos-cmd))
  (setq soscmd (concat soscmd " "
                       op ))
  (setq soscmd (concat soscmd " "
                       file-list))
  (pcase op
    ("ci" (progn
            (setq soscmd (concat soscmd " "
                                 "-aLog=\""
                                 (read-string "CI Log:")
                                 "\""))
            (shell-command soscmd)))
    ("modattr" (progn
                 (setq soscmd (concat soscmd " "
                                      "-a"
                                      (helm-comp-read "Arttribute: " sos-attribute) "="
                                      (helm-comp-read "Arttribute\'s value: " sos-attribute-value)
                                      ))
                 (shell-command soscmd)
                 ))
    (_ (shell-command soscmd)))
  (message soscmd)
  (revert-buffer)
  )

;;;###autoload
(define-minor-mode sos-mode
  "commands used in the version control system SOS"
  :group 'sos-mode
  :init-value nil
  :lighter "SOS "
  :global nil
  :after-hook nil
  :global t

  )
