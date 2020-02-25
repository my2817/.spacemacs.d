;;;
;;; Variables
;;;
(setq frame-title-format
      '("GNU/Emacs - [ "
        (or (file-remote-p default-directory 'user)
            user-real-login-name)
        "@" system-name ":"
        (buffer-file-name "%f \]"
                          (dired-directory dired-directory "%b \]"))))

(defcustom graphviz-dot-mode-hook nil
  "Hook run after graphviz-dot-mode is loaded"
  :type 'hook
  :group 'graphviz-dot-mode)

(with-eval-after-load 'mmm-mode
  (progn

    (mmm-add-mode-ext-class 'verilog-mode nil 'verilog-org )
    (mmm-add-classes
     '((verilog-org
        :submode org-mode
        :front "\\/\\*-- \\(org\\).*\n"
        :back "^.*\\(!org\\) --\\*\\/")))

    ;; in multi-line comment block, we use org-mode as default
    ;; (mmm-add-mode-ext-class 'verilog-mode nil 'verilog-org-1 )
    (mmm-add-classes
     '((verilog-org-1
        :submode org-mode
        :front "\\/\\*[\\*-=]*\n"
        :back "^.*[\\*-=]*\\*\\/")))

    (mmm-add-mode-ext-class 'verilog-mode nil 'verilog-lisp )
    (mmm-add-classes
     '((verilog-lisp
        :submode lisp-interaction-mode
        :front "\\/\\*-- \\(lisp\\)"
        :back "\\(!lisp\\) --\\*\\/")))

    (mmm-add-mode-ext-class 'verilog-mode nil 'verilog-dot )
    (mmm-add-classes
     '((verilog-dot
        :submode graphviz-dot-mode
        :front "\\/\\*-- \\(dot\\)"
        :back "\\(!dot\\) --\\*\\/")))
    (mmm-add-mode-ext-class 'html-mode nil 'html-java)
    (mmm-add-classes
     '((html-java
        :submode javascript-mode
        :front "<script type=\"text/javascript\">"
        :back "</script>")))))

(add-to-list 'page-break-lines-modes 'verilog-mode)
(setq evil-want-abbrev-expand-on-insert-exit nil)
;;;
;;; mode on/off
;;;
(global-hl-line-mode -1)
;; (custom-set-faces
;;  '(hl-line ((t (:box (:line-width 1 :color "grey75" :style pressed-button))))))
(blink-cursor-mode 1)
;; (electric-pair-mode -1)

;;;
;;; file extensions
;;;
(add-to-list 'auto-mode-alist '("\\.log\\'" . compilation-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.list\\'" . text-mode))

;;;
;;; Hooks
;;;
;; (add-hook 'verilog-mode-hook 'auto-complete-mode)
(add-hook 'verilog-mode-hook 'company-mode)
(add-hook 'verilog-mode-hook 'mmm-mode)
(add-hook 'verilog-mode-hook 'electric-spacing-mode)
(add-hook 'graphviz-dot-mode-hook 'smartparens-mode)
(add-hook 'c++-mode-hook 'electric-spacing-mode)
(add-hook 'perl-mode-hook 'electric-spacing-mode)
(add-hook 'c-mode-hook 'electric-spacing-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda ()
                              (interactive)
                              (if (string= mode-name "Verilog")
                                  (progn
                                    (untabify (point-min)
                                              (point-max))
                                    ))))
;;(add-hook 'verilog-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'makefile-bsdmake-mode-hook (lambda ()
                                        (company-mode)
                                        (setq-local company-backends '(company-files
                                                                       (company-dabbrev-code company-gtags company-etags company-keywords)
                                                                       company-oddmuse company-dabbrev))))
(add-hook 'makefile-gmake-mode-hook (lambda ()
                                        (company-mode)
                                        (setq-local company-backends '(company-files
                                                                       (company-dabbrev-code company-gtags company-etags company-keywords)
                                                                       company-oddmuse company-dabbrev))))
(add-hook 'text-mode-hook (lambda ()
                                      (company-mode)
                                      (setq-local company-backends '(company-files
                                                                     (company-dabbrev-code company-gtags company-etags company-keywords)
                                                                     company-oddmuse company-dabbrev))))
(add-hook 'conf-mode-hook (lambda ()
                            (spacemacs/toggle-relative-line-numbers-on)))
(add-hook 'company-mode-hook 'company-posframe-mode)

(mapc #'wttr/prepend-to-exec-path
      (reverse
       '("D:/Perl/bin"
         "D:/EDA/graphviz-2.38/bin"
         "D:/cygwin64/bin"
         "D:/EDA/modeltech64_10.2c/win64"
         "d:/EDA/Git-2.6.0-32-bit/PortableGit/usr/bin"
         "C:/Program\ Files\ (x86)/Java/jre1.8.0_171/bin"
         "D:/EDA/Git-2.6.0-32-bit/PortableGit/bin"
         "D:/EDA/libxml2/iconv-1.9.2.win32/bin"
         "D:/EDA/libxml2/iconv-1.9.2.win32/lib"
         "D:/EDA/libxml2/libxml2-2.7.8.win32/bin"
         "D:/EDA/libxml2/libxml2-2.7.8.win32/lib"
         "D:/EDA/libxml2/zlib-1.2.5/bin"
         "D:/EDA/libxml2/zlib-1.2.5/lib"
         "C:/ProgramData/Oracle/Java/javapath"
         "e:/Git-2.6.0-32-bit/PortableGit/usr/bin"
         "e:/Git-2.6.0-32-bit/PortableGit/bin"
         )))

(if (string-equal system-type "windows-nt")
    (setenv "GRAPHVIZ_DOT" (find-file-in-path-list 'dot.exe exec-path)))

;; (defvar my-grep-global-ignore-dired
;;       '(
;;         ".SOS"
;;         "irun.*"
;;         "INCA.*"
;;         ))
;; (defvar my-grep-global-ignore-files
;;       '(
;;         "*.txt"
;;         "irun.*"
;;         "*.log"
;;         "*.dsn"
;;         "*.pak"
;;         "*.pvl"
;;         "*.syn"
;;         "*.mr"
;;         ))
;; (with-eval-after-load 'helm-grep
;;   (mapcar
;;    (lambda (item)
;;      (add-to-list 'helm-grep-ignored-directories item))
;;    my-grep-global-ignore-dired)
;;   (mapcar
;;    (lambda (item)
;;      (add-to-list 'helm-grep-ignored-files item))
;;    my-grep-global-ignore-files))
;; (with-eval-after-load 'grep
;;   (mapcar
;;    (lambda (item)
;;      (add-to-list 'grep-find-ignored-directories item))
;;    my-grep-global-ignore-dired)
;;   (mapcar
;;    (lambda (item)
;;      (add-to-list 'grep-find-ignored-files item))
;;    my-grep-global-ignore-files))

;; projectile
;; (defvar my-project-globally-ignored-files
;;   '(
;;     "*.pak$"
;;     )
;;   )
;; (with-eval-after-load 'projectile
;;   (mapcar
;;    (lambda (item)
;;      (add-to-list 'projectile-globally-ignored-files item))
;;    my-project-globally-ignored-files)
;;   )
(setq enable-local-variables :all enable-local-eval t)

;; the following var are required by zilongshanren-org/post-init-org
(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(setq-default
 org-agenda-dir "~/org-notes"
 deft-dir "~/org-notes"
 blog-admin-dir "~/4gamers.cn")

;; (with-eval-after-load 'org-agenda
;;   (require 'org-projectile)
;;   ;; (mapcar (lambda (f)
;;   ;;           (if (file-exists-p f)
;;   ;;               (add-to-list 'org-agenda-files f)))
;;   ;;         (org-projectile-todo-files)
;;   ;;         )
;;   (org-projectile/update-agenda-files)
;;   )

;; (with-eval-after-load 'auto-complete
;;   (add-hook 'minibuffer-setup-hook
;;                (lambda ()
;;                  (if (string= major-mode "verilog-mode")
;;                      (add-to-list 'ac-sources 'ac-source-words-in-all-buffer)
;;                    (setq ac-sources (remove 'ac-source-words-in-all-buffer ac-sources)))))
;;   (defcustom ac-in-minibuffer nil
;;     "Non-nil means expand in minibuffer."
;;     :type 'boolean
;;     :group 'auto-complete)

;;   (defun ac-handle-post-command ()
;;     (condition-case var
;;         (when (and ac-triggered
;;                    (not (ido-active)) ;; Disable auto pop-up in ido mode
;;                    (or ac-auto-start
;;                        ac-completing)
;;                    (not isearch-mode))
;;           (setq ac-last-point (point))
;;           (ac-start :requires (unless ac-completing ac-auto-start))
;;           (ac-inline-update))
;;       (error (ac-error var))))

;;   (defun auto-complete-mode-maybe ()
;;     "What buffer `auto-complete-mode' prefers."
;;     (if (or (and (minibufferp (current-buffer)) ac-in-minibuffer) ;; Changed
;;             (memq major-mode ac-modes))
;;         (auto-complete-mode 1)))

;;   )

(with-eval-after-load 'tramp
  (add-to-list 'tramp-methods
               '("my-ssh"
                 (tramp-login-program        "ssh")
                 (tramp-login-args           (
                                              ("-o" "PreferredAuthentications=password")
                                              ("-l" "%u") ("-p" "%p") ("%c")
                                              ("-e" "none") ("%h")
                                              ))
                 (tramp-async-args           (("-q")))
                 (tramp-remote-shell         "/bin/sh")
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-gw-args              (
                                              ("-o" "GlobalKnownHostsFile=/dev/null")
                                              ("-o" "UserKnownHostsFile=/dev/null")
                                              ("-o" "StrictHostKeyChecking=no")
                                              )
                                             )
                 (tramp-default-port         22)))
  )

(with-eval-after-load 'persp-mode
  (setq persp-auto-save-opt 0)
  )

(with-eval-after-load 'yasnippet-snippets
  (add-to-list 'yas-snippet-dirs "~/.spacemacs.d/layers/my-config/local/my-verilog/snippets")
  (yas-load-directory "~/.spacemacs.d/layers/my-config/local/my-verilog/snippets" t)
  )

(setq epa-pinentry-mode 'loopback)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
