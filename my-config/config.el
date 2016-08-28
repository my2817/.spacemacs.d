;;;
;;; Variables
;;;
(setq frame-title-format
      '("GNU/Emacs - [ "
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
        :front "\\/\\*-- \\(org\\)"
        :back "\\(!org\\) --\\*\\/")))


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

;;;
;;; mode on/off
;;;
(global-hl-line-mode -1)
(blink-cursor-mode 1)
(electric-pair-mode 1)

;;;
;;; file extensions
;;;
(add-to-list 'auto-mode-alist '("\\.log\\'" . compilation-mode))

;;;
;;; Hooks
;;;
(add-hook 'verilog-mode-hook 'global-auto-complete-mode)
;;(add-hook 'verilog-mode-hook 'mmm-mode)
(add-hook 'verilog-mode-hook 'electric-spacing-mode)
(add-hook 'graphviz-dot-mode-hook 'smartparens-mode)
(add-hook 'graphviz-dot-mode-hook 'smartparens-mode)
(add-hook 'c++-mode-hook 'electric-spacing-mode)
(add-hook 'perl-mode-hook 'electric-spacing-mode)
(add-hook 'c-mode-hook 'electric-spacing-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook (lambda ()
                              (interactive)
                              (untabify (point-min)
                                        (point-max))))
(add-hook 'verilog-mode-hook 'turn-on-ctags-auto-update-mode)

(mapc #'wttr/prepend-to-exec-path
      (reverse
       '("D:/Perl/bin"
         "D:/EDA/graphviz-2.38/bin"
         "D:/cygwin/bin"
         "D:/EDA/modeltech64_10.2c/win64"
         "D:/EDA/Git-2.6.0-32-bit/PortableGit/bin"
         )))
