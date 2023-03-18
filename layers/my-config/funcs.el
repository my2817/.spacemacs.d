(defun wttr/prepend-to-exec-path (path)
  "prepand the path to the emacs intenral `exec-path' and \"PATH\" env variable.
    Return the updated `exec-path'"
  (let ((file-path (expand-file-name path))
        (env-path (getenv "PATH"))
        )
    ;; avoid dumplicate
    (setq env-path (replace-regexp-in-string file-path "" env-path))
    (setq env-path (replace-regexp-in-string ":+" ":" env-path))

    (setenv "PATH" (concat file-path
                           path-separator
                           env-path))

    ;; avoid dumplicate
    (setq exec-path (delete file-path exec-path))
    (setq exec-path
          (cons file-path
                exec-path))))

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
    (let ((process-connection-type nil)
          ;; refer to following variables/funcs to get right desktop environment
          ;; `spacemacs-ignored-environment-variables'
          ;; `spacemacs/force-init-spacemacs-env'
          (desktop (getenv "XDG_CURRENT_DESKTOP")))
      (cond ((string-equal "GNOME" desktop)
             (start-process "" nil "gnome-terminal"
                            (concat "--working-directory=" default-directory)
                            "--profile=zsh"))
            ((string-equal "XFCE" desktop)
             (start-process "" nil "xfce4-terminal"
                            (concat "--default-working-directory=" default-directory)))
            (t
             (message "Error:unknown desktop environment of \"XDG_CURRENT_DESKTOP\":%s" desktop)))))))

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
         (fn-dir-local (concat (projectile-project-root) ".dir-locals.el"))
         (fn-ctags-options (concat (projectile-project-root) ".ctags.d/options.ctags"))
         )
    (if (and (projectile-project-p)
             (not (file-exists-p fn-dir-local))
             )
        (progn
          (with-temp-file fn-dir-local
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
                         )))))
")
           )
          (if (not (file-exists-p (file-name-directory fn-ctags-options)))
              (make-directory (file-name-directory fn-ctags-options)))
          (with-temp-file fn-ctags-options
            (insert "
--links=yes
--langdef=VerilogExt{base=Verilog}
--kinddef-VerilogExt=t,tags, tags defined by begin:
--regex-VerilogExt=/begin: *([a-zA-Z0-9_]*)/\\1/t

--exclude-exception=*.lib

--langdef=Liberty
--map-Liberty=+.lib
--kinddef-Liberty=l,library, name of liberty library
--kinddef-Liberty=c,cell, name of liberty cell
--regex-Liberty=/^library *\\\( *\"?([0-9A-Za-z_]*)\"? *\\\)/\\1/l
--regex-Liberty=/^ *cell *\\\( *\"?([a-zA-Z0-9_]*)\"? *\\\)/\\1/c

")
            ))
      (message ".dir-locals exists: %s" fn-dir-local)
      )))

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

;; (defadvice org-edit-src-exit (after restore-window-config activate)
;;   (winner-undo))
;; (ad-activate 'org-edit-src-exit)


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
  (require 'popup)
  (let* ((local-word (replace-regexp-in-string "^[0-9]?+'?[bB]" "" (thing-at-point 'symbol 'no-properties)))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "bin2hex: %s -> %d" (symbol-at-point) (string-to-number  local-word 2)))
    (popup-tip (format "dec:%d"  (string-to-number  local-word 2)) :point (point) :nowait nil))
  )

(defun my-bin2hex(&optional arg)
  (interactive "P")
  (require 'popup)
  (let* ((local-word (replace-regexp-in-string "^[0-9]?+'?[bB]" "" (thing-at-point 'symbol 'no-properties)))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "bin2hex: %s -> %x" (symbol-at-point) (string-to-number  local-word 2)))
    (popup-tip (format "hex:%x"  (string-to-number  local-word 2)) :point (point) :nowait nil) )
  )

(defun my-bin2xx(&optional arg)
  (interactive "P")
  (require 'popup)
  (let* ((local-word (replace-regexp-in-string "^[0-9]?+'?[bB]" "" (thing-at-point 'symbol 'no-properties)))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "bin2hex: %s -> %x" (symbol-at-point) (string-to-number  local-word 2)))
    (popup-tip (format "dec:%d\nhex:%x"
                       (string-to-number local-word 2)
                       (string-to-number  local-word 2)) :point (point) :nowait nil) )
  )

(defun my-dec2hex(&optional arg)
  "C-0 my-dec2hex to accepts user input"
  (interactive "P")
  (require 'popup)
  (let* ((local-word (if arg
                         (ivy-completing-read "input dec: " nil)
                       (replace-regexp-in-string "^[0-9]?+'?[dD]" "" (thing-at-point 'word 'no-properties))))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "dex2hex : %s -> %x" (symbol-at-point) (string-to-number  local-word 10)))
    (popup-tip (format "hex:%x"  (string-to-number  local-word 10)) :point (point) :nowait nil) )
  )

(defun my-dec2bin(&optional arg)
  "C-0 my-dec2hex to accepts user input"
  (interactive "P")
  (require 'popup)
  (let* ((local-word (if arg
                         (ivy-completing-read "input dec: " nil)
                       (replace-regexp-in-string "^[0-9]?+'?[dD]" "" (thing-at-point 'word 'no-properties))))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "dex2bin : %s -> %s" (symbol-at-point) (my-numb2bin (string-to-number  local-word 10))))
    (popup-tip (format "bin:%s"  (my-numb2bin (string-to-number  local-word 10))) :point (point) :nowait nil) )
  )

(defun my-numb2bin (n)
  "Number to binnay string"
  (interactive)
  (let* ((i n)
         (j 0)
         (str nil))
    (while (> i 0)
      (if (and (= (% j 4) 0)
               (not (= j 0)))
          (setq str (concat "_" str)))
      (setq str (concat (number-to-string (% i 2)) str))
      (setq i (/ i 2))
      (setq j (+ j 1))
      )
    str))

(defun my-hex2dec(&optional arg)
  "C-0 my-hex2dec to accepts user input"
  (interactive "P")
  (require 'popup)
  (let* ((local-word (if arg
                         (ivy-completing-read "input hex: " nil)
                       (replace-regexp-in-string "^[0-9]?+'?[xXhH]" "" (thing-at-point 'word 'no-properties))))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "hex2dec : %s -> %d" (symbol-at-point) (string-to-number  local-word 16)))
    (popup-tip (format "dec:%d"  (string-to-number  local-word 16)) :point (point) :nowait nil) )
  )

(defun my-hex2bin(&optional arg)
  "C-0 my-hex2dec to accepts user input"
  (interactive "P")
  (require 'popup)
  (let* ((local-word (if arg
                         (ivy-completing-read "input hex: " nil)
                       (replace-regexp-in-string "^[0-9]?+'?[xXhH]" "" (thing-at-point 'word 'no-properties))))
         (local-word (replace-regexp-in-string "_" "" local-word)))
    (message (format "hex2bin : %s -> %s" (symbol-at-point) (my-numb2bin (string-to-number  local-word 16))))
    (popup-tip (format "bin:%s"  (my-numb2bin (string-to-number  local-word 16))) :point (point) :nowait nil) )
  )

(defun my-yank-image-from-win-clipboard-through-powershell()
  "to simplify the logic, use c:/Users/Public as temporary directoy, the move it into current directoy

fix bug, save clipboard immediately, so that we can give new name from clipboard again
"
  (interactive)
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (file-name (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))
         (file-path-wsl  "./images/"))
    (if (file-exists-p "./images")
        (ignore)
      (make-directory "./images"))
    ;; (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/\\$env:USERNAME/" file-name "\\\")\""))
    (shell-command (concat powershell " -command \"(Get-Clipboard -Format Image).Save(\\\"C:/Users/Public/" file-name "\\\")\""))
    (setq file-path-wsl (concat file-path-wsl (read-from-minibuffer "Img Name:" file-name)))
    (rename-file (concat "/mnt/c/Users/Public/" file-name)
                 file-path-wsl)
    (format "%s" file-path-wsl)
    ))

(defun my-yank-image-link-into-org-from-wsl ()
  "call `my-yank-image-from-win-clipboard-through-powershell' and insert image file link with org-mode format"
  (interactive)
  (let* ((file-path (my-yank-image-from-win-clipboard-through-powershell))
         (des (read-string "Description:" (file-name-sans-extension (file-name-nondirectory file-path))))
         )
    (if (string= des "")
        (insert (format "[[file:%s]]" file-path ))
      (insert (format "[[file:%s][%s]]" file-path des)))
    ))

;; (defun xah-open-in-desktop-from-wsl()
;;   "open desktop by send command from wsl into powershell"
;;   (interactive)
;;   (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
;;          (directory (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" default-directory))
;;          )
;;     (shell-command (concat powershell " -command \"start " directory "\""))
;;     )
;;   )

;; (defun xah-open-in-xternal-app-from-wsl()
;;   "open desktop by send command from wsl into powershell"
;;   (interactive)
;;   (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
;;          (directory (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" default-directory))
;;          (-file-list
;;           (if (string-equal major-mode "dired-mode")
;;               (dired-get-marked-files)
;;             (list (buffer-file-name))))
;;          )
;;     (shell-command (concat powershell " -command \"start " (replace-regexp-in-string "/mnt/\\([a-zA-Z]\\)" "\\1:" (nth 0 -file-list)) "\""))
;;     )
;;   )

;;;###autoload

(defmacro wsl--open-with (id &optional app dir)
  `(defun ,(intern (format "wsl/%s" id)) ()
     (interactive)
     (wsl-open-with ,app ,dir)))

(defun wsl-open-with (&optional app-name path)
  "Send PATH to APP-NAME on WSL."
  (interactive)
  (let* ((path (expand-file-name
                (replace-regexp-in-string
                 "'" "\\'"
                 (or path (if (derived-mode-p 'dired-mode)
                              (dired-get-file-for-visit)
                            (buffer-file-name)))
                 nil t)))
         (command (format "%s `wslpath -w %s`" (shell-quote-argument app-name) path)))
    (shell-command-to-string command)))

(wsl--open-with open-in-default-program "explorer.exe" buffer-file-name)
(wsl--open-with reveal-in-explorer "explorer.exe" default-directory)


(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:n” that indicates line number, or “:n:m” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
         ($inputStr
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let ($p0 $p1 $p2
                      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»❮❯❬❭〘〙·。()+\\"))
              (setq $p0 (point))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (let (($x (xahsite-url-to-filepath $path)))
              (if (string-match "^http" $x )
                  (browse-url $x)
                (find-file $x)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "#" $path )
            (let (
                  ( $fpath (substring $path 0 (match-beginning 0)))
                  ( $fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char (point-min))
                    (search-forward $fractPart ))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\)[:,]\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char (point-min))
                      (forward-line (1- $line-num)))
                  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file no exist: 「%s」. Create?" $path))
                  (find-file $path ))))))))))


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

(with-eval-after-load 'dired
  (define-key dired-mode-map "e" 'ora-ediff-files))
(defun ora-ediff-files ()
  "
    In case no files are marked, the file at point is used as the first file, and read-file-name is used for the second file. Since I have the magic (setq dired-dwim-target t) in my config, in case a second dired buffer is open, dired-dwim-target-directory will offer it as the starting directory during completion. Very useful to compare two files in two different directories.

    Depending on the order of the arguments to ediff-files, the changes will appear either as added or removed; file-newer-than-file-p tries to put the arguments in a logical order by looking at the files' last change times.

    ediff-after-quit-hook-internal is used to restore the previous window configuration after I quit ediff with q.
"
  (interactive)
  (require 'dired-aux)
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

(defun my-highlight-symbol-in-region ()
  "run `highlight-symbol' go through selected region"
  (interactive)
  (let* ((p-region-start (and (region-active-p) (region-beginning)))
         (p-region-end (and (region-active-p) (region-end))))
    (if (not (region-active-p))
        (highlight-symbol)
      (save-excursion
        (while (and (forward-symbol 1)
                    (< (point) p-region-end))
          (highlight-symbol))))))

(defun my-insert-indices ()
  "Insert a set of generated numbers into a rectangle."
  (interactive)
  (let (
        (n    (read-number "the indices should start at:" 0))
        (max  (read-number "the indices should end at:" 3))
        (step (read-number "the step betweeen indices:" 1))
        (fmt  (read-string "format of indices:" "%0d"))
        (col  (current-column))
        )
    (save-excursion
      (while (<= n max)
        (insert (format fmt n))
        (forward-line 1)
        (if (eobp)
            (progn
             (setq n (+ step max))
             (message "End of buffer, can't insert more indics..."))
          (setq n (+ step n)))
          (move-to-column col t)))))

(defun my-repeat-current-line-with-indices ()
  "Insert a set of generated numbers into a rectangle."
  (interactive)
  (let (
        (n    (read-number "the indices should start at:" 0))
        (max  (read-number "the indices should end at:" 3))
        (step (read-number "the step betweeen indices:" 1))
        (fmt  (read-string "format of indices:" "%0d"))
        (col  (current-column))
        (line (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
        )
    (save-excursion
      (while (<= n max)
        (insert (format fmt n))
        (goto-char (point-at-eol))
        (and (< n max)
             (insert (format "\n%s" line)))
        (goto-char (point-at-bol))
        (move-to-column col t)
        (setq n (+ n 1))))))

(defun qr-code-region (start end)
  "Show a QR code of the region.
A simple way to transfer text to the phone...
https://www.emacswiki.org/emacs/QR_Code"
  (interactive "r")
  (let ((buf (get-buffer-create "*QR*"))
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer))
    (let ((coding-system-for-read 'raw-text))
      (shell-command-on-region start end "qrencode -o -" buf))
    (switch-to-buffer buf)
    (image-mode)))

(defun my-exwm-rename-buffer()
  "search porject name in title if it exist.
use last 40 chars as buffer name if the title name is too long.
"

  (let* (
         ;; (maxlen 40)
         (fn-pos (when (string-match "[~.]?/[0-9a-zA-Z/_+-.]+" exwm-title)
                   (match-data)))
         (fn (when fn-pos
               (substring exwm-title (car fn-pos) (cadr fn-pos))))
         ;;project-root
         (pr (when (projectile-project-p fn)
               ;; remove last "/" if exist
               (replace-regexp-in-string "/$" "" (projectile-project-root fn))))
         ;; project-name
         (pn (when pr
               (projectile-project-name pr)))
         (collapse-title (when pn
                           (replace-regexp-in-string pr (concat "<" pn ">") exwm-title)))
         )
    (exwm-workspace-rename-buffer (if (not collapse-title)
                                      ;; (if (> (length exwm-title) maxlen)
                                      ;;     (concat exwm-class-name "|" (subseq exwm-title (- 0 maxlen)))
                                      ;;   (concat exwm-class-name "|" exwm-title)
                                      ;;   )
                                      (concat exwm-class-name "|" exwm-title)
                                    (concat exwm-class-name "|" collapse-title)))))

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
;; (define-key ctl-x-4-map "t" 'toggle-window-split)

(define-advice dired-do-print (:override (&optional _))
  ;; 修改自 https://www.emacswiki.org/emacs/DiredOmitMode
  "Show/hide dotfiles. Bind to \"P\" in dired-mode"
  (interactive)
  (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
      (progn
        (setq-local dired-dotfiles-show-p nil)
        (dired-mark-files-regexp "^\\.")
        (dired-do-kill-lines))
    (revert-buffer)
    (setq-local dired-dotfiles-show-p t)))

(defun my-insert-change-log ()
" Inter change log from the result of `my-parse-git-diff'
"
  (interactive)
  (let* ((changes (my-parse-git-diff)))
    (dolist (file-changes changes)
      (insert (car file-changes))
      (insert "\n")
      (dolist (change (cadr file-changes))
        (insert (format "    %s\n" change))))))

(defun my-parse-git-diff ()
  "Parse change log from output of \"git diff\"
the change log should be style with :
 // 2020-12-14 comment string
or
 // 2020-12-14 12:12:12 comment string

Return alist with structure: '( (fn1 (log-str log-str)) (fn2 (log-str log-str)) )
"
  (let* ((fn nil)
         (log-strings)
         (file-result);; '(fn (log-str1 ...))
         (alist (process-lines  "git" "--no-pager" "diff" "--no-color" "-U0" "--cached")))
    (dolist (line alist)
      (and (string-prefix-p "diff --git a/" line)
           (or (and fn
                    (add-to-list 'file-result (list fn log-strings)))
               t)
           ;; save filename
           (setq fn (substring (nth 2 (split-string line)) 2))
           (setq log-strings '()))
      (and (string-match (concat "\\(//\\s-?+"
                                 (format-time-string "%Y-%m-%d");; date 2020-12-14
                                 "\\)"
                                 "\\(\\s-?+[0-9:]+\\)?";; time h:m:s
                                 "\\s-?\\(.*$\\)") line)
           (add-to-list 'log-strings (match-string 3 line))))
    (add-to-list 'file-result (list fn log-strings))))

(defun et/rename-org-link-file (path)
  ;; https://emacs-china.org/t/gist-org-link/16860/10?u=my2817
  (interactive
   (list
    (let* ((link (org-element-context))
           (old-path (org-element-property :path link)))
      (read-string "PATH: " old-path nil old-path))))
  (let* ((link (org-element-context))
         (old-path (org-element-property :path link))
         (ltype (org-element-property :type link)))
    (if (and (file-exists-p old-path)
             (member ltype '("file" "docview" "attachment")))
        (progn (mkdir (file-name-directory path) t)
               (rename-file old-path path)
               (save-excursion
                 (goto-char (point-min))
                 (while (search-forward old-path nil t)
                   (replace-match path)))
               (message (format "Rename %s to %s" old-path path)))
      (message (format "!!!File type not valid or dose not exist!!! %s %s" ltype old-path)))))
(require 'xref)
(defun my--push-point-to-xref-marker-stack (&rest r)
  "You can jump back with any of them type \\[pop-tag-mark],

For those who are using Citre with other tools (imenu, grep...)"
  (xref-push-marker-stack (point-marker)))
(dolist (func '(find-function
                counsel-imenu
                helm-imenu
                projectile-grep
                helm-grep-ag
                counsel-rg
                lsp-ivy-workspace-symbol
                citre-jump
                avy-goto-word-or-subword-1
                find-file
                swiper
                evil-goto-definition
                ;; evil-previous-line
                ;; evil-next-line
                evil-avy-goto-word-or-subword-1
                ;; backward-paragraph
                ;; forward-paragraph
                evil-goto-first-line
                evil-goto-line
                ))
  (advice-add func :before 'my--push-point-to-xref-marker-stack))

(with-eval-after-load 'ediff
 (defun ediff-make-temp-file (buff &optional prefix given-file start end)
   (let* ((p (ediff-convert-standard-filename (or prefix "ediff")))
          (short-p p)
          (coding-system-for-write ediff-coding-system-for-write)
          f short-f)
     (if (and (fboundp 'msdos-long-file-names)
              (not (msdos-long-file-names))
              (> (length p) 2))
         (setq short-p (substring p 0 2)))

     (setq f (concat ediff-temp-file-prefix p)
           short-f (concat ediff-temp-file-prefix short-p)
           f (cond (given-file)
                   ((find-file-name-handler f 'insert-file-contents)
                    ;; to thwart file handlers in write-region, e.g., if file
                    ;; name ends with .Z or .gz
                    ;; This is needed so that patches produced by ediff will
                    ;; have more meaningful names
                    (ediff-make-empty-tmp-file short-f))
                   (prefix
                    ;; Prefix is most often the same as the file name for the
                    ;; variant.  Here we are trying to use the original file
                    ;; name but in the temp directory.
                    (ediff-make-empty-tmp-file f 'keep-name))
                   (t
                    ;; If don't care about name, add some random stuff
                    ;; to proposed file name.
                    (ediff-make-empty-tmp-file short-f))))

     ;; create the file
     (ediff-with-current-buffer buff
       (write-region (if start start (point-min))
                     (if end end (point-max))
                     f
                     nil          ; don't append---erase
                     'no-message)
       (set-file-modes f ediff-temp-file-mode)
       (expand-file-name f))
     ;; if get a gpg file which can be decided by the name style of buffer name
     ;; 1. append ".gpg" to temp file by rename-file
     ;; 2. decode gpg file by find-file
     ;; 3. copy decoded gpg file's content by append-to-buff
     ;; 4. save decoded buffer's content to tmpfile
     ;; 5. return tmpfile name
     (if (string-match "org.gpg" (or (and (bufferp buff)
                                          (buffer-name buff))
                                     buff))
         (if (string-match "org.gpg$" (buffer-name buff))
             (progn ;; file has been decoded, write buffer's conntent to
               (let* ((f (ediff-make-empty-tmp-file short-f)))
                 (get-buffer-create "*New-decode*")
                 (switch-to-buffer buff)
                 (append-to-buffer "*New-decode*" (point-min) (point-max))
                 (write-file f nil)
                 (kill-buffer (get-buffer "*New-decode*"))
                 (expand-file-name f)
                 ))
           (with-current-buffer buff
             (read-only-mode 0)
             (erase-buffer)
             (let* ((f-gpg (format "%s.org.gpg" f))
                    (buff-decode (format "%s.gpg" (file-name-base f-gpg)))
                    (file-decode (file-name-sans-extension f-gpg)))
               (rename-file f f-gpg)
               (find-file f-gpg)
               (delete-file f-gpg)
               (recentf-remove-if-non-kept f-gpg)
               (write-file file-decode nil)
               (append-to-buffer buff (point-min) (point-max))
               (kill-buffer (get-buffer buff-decode))
               (switch-to-buffer buff)
               (expand-file-name file-decode))))
       (expand-file-name f))
     )))

(with-eval-after-load 'git-gutter
  (defun git-gutter--turn-on ()
    (when (and (buffer-file-name)
               (not (memq major-mode git-gutter:disabled-modes))
               (not (string-match (purecopy "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'") buffer-file-name)))
      (git-gutter-mode +1)))
  )

(defun my-hideshowvis-fringe ()
  (interactive )
  (and (string= (string (char-syntax (char-before)))
                "w")
       (string= (string (char-syntax (char-after)))
                "w")
       (forward-word))
  (if (hs-already-hidden-p)
      (hs-show-block)
    (hs-hide-block)))

(defun my-indent/hs ()
  "work around `indent-for-tab-command', indent or hide/show fring.
If `buffer-read-only' is non-nil, execute `my-hideshowvis-fringe'.
If `electric-verilog-tab' don't change position, execute `my-hideshowvis-fringe'.
"
  (interactive)
  (if (or buffer-read-only
          (let* ((old-position (point))
                 (new-position (progn (indent-for-tab-command)
                                      (point))))
            (= old-position new-position))
          )
      (my-hideshowvis-fringe)))

(defadvice  persp-load-state-from-file (after my-persp-load-state-from-file)
  (spacemacs/persp-perspectives))
(ad-activate 'persp-load-state-from-file)

(define-key global-map [remap indent-for-tab-command] 'my-indent/hs)

(defadvice pop-tag-mark (after my-pop-tag-mark)
  (recenter))
(ad-activate 'pop-tag-mark)
