;;; packages.el --- my-config layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: heyun <heyun@localhost.localdomain>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `my-config-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `my-config/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `my-config/pre-init-PACKAGE' and/or
;;   `my-config/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst my-config-packages
  '(
    auto-complete
    ;; graphviz-dot-mode ;; it has been configured by graphviz layer
    ;blink-cursor-mode
    ;; psvn
    (my-verilog :location local)
    ;;(mmm-mode :location local)
    mmm-mode
    highlight-symbol
    (tcl-dc-mode :location local)
    ;htmlize
    electric-spacing
    ;; ctags
    ctags-update
    ;; auctex
    ;; auto-complete-auctex
    plantuml-mode
    ;; tabbar
    ;; tabbar-ruler
    (sos-mode :location local)
    (org :location built-in)
    (compilation-mode :location built-in)
    flycheck
    flycheck-plantuml
    (company-verilog :location local)
    company
    (company-english-helper :location local)
    ;; (awesome-tab :location local);;from [https://github.com/manateelazycat/awesome-tab]
    ;; header2
  )

  "The list of Lisp packages required by the my-config layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun my-config/post-init-auto-complete()
  "initial my-config/auto-complete"
  (use-package auto-complete
    :defer t
    :config
    (progn
      ;; (global-auto-complete-mode 1)
      (add-to-list 'ac-modes 'graphviz-dot-mode)
      (add-to-list 'ac-modes 'makefile-gmake-mode)
      (add-to-list 'ac-modes 'TeX-latex-mode)
      (add-to-list 'ac-modes 'verilog-mode)
      (add-to-list 'ac-modes 'tcl-dc-mode)
      (add-to-list 'ac-dictionary-directories (concat
                                               (or (file-name-directory #$) (car load-path))
                                               "local/ac-dict/"))
      (add-to-list 'ac-sources 'ac-source-filename)
      (add-to-list 'ac-sources 'ac-source-abbrev)
      (setq ac-source-time
            '((candidates . (list (format "%s" (format-time-string "%Y-%m-%d-%H:%M:%S"))))
              (prefix . "@\\(.*\\)")))
      (add-to-list 'ac-sources 'ac-source-time)
      ;;(add-to-list 'ac-dictionary-directories (expand-file-name "local/ac-dict/" user-emacs-directory))
      (setq-default ac-sources ac-sources);;make sure the default value is what I seeted previous
      (setq-default ac-disable-faces nil))))

;; (defun my-config/init-graphviz-dot-mode ()
;;   "Initialize my package"
;;   (use-package graphviz-dot-mode
;;     :defer t
;;     :init
;;     (progn
;;       (setq graphviz-dot-preview-extension "jpg")
;;       )))
;; (defun my-config/init-psvn ()
;;   "Initialize psvn"
;;   (use-package psvn
;;     :defer t
;;     :init
;;     ))

(defun my-config/init-htmlize ()
  "Initialize htmlize"
  (use-package htmlize
    :defer t
    ))
(defun my-config/init-my-verilog ()
  "Initialize my package"
  (use-package my-verilog
    :defer t
    :init
    (progn
      ;; (require 'my-verilog)
      (autoload 'my-verilog "my-verilog" "my configuration of verilog-mode " t)
      (add-hook 'verilog-mode-hook 'my-verilog)
    )
    ))

(defun my-config/post-init-mmm-mode ()
  "Initialize my package"
  (use-package mmm-mode
    :defer t
    :config
    (progn
      ;;(autoload 'mmm-mode "mmm-mode" "Multiple Major Modes" t)
      ;;(autoload 'mmm-parse-buffer "mmm-mode" "Automatic MMM-ification" t)
      (setq mmm-global-mode 'maybe))
    ))

(defun my-config/init-highlight-symbol ()
  "Initialize my package"
  (use-package highlight-symbol
    :defer t
    :init
    (progn
      ;;(require 'highlight-symbol)
      ;;(autoload 'highlight-symbol "highlight-symbol" "Highlight-Func Mode" t)
      )
    ))

(defun my-config/init-tcl-dc-mode ()
  "Initialize my package"
  (use-package tcl-dc-mode
    :defer t
    :init
    (progn
      (autoload 'tcl-dc-mode "tcl-dc-mode" "Tcl DC Mode" t)
      (add-to-list 'auto-mode-alist '("\\.tcl\\'" . tcl-dc-mode))
      (add-to-list 'auto-mode-alist '("\\.sdc\\'" . tcl-dc-mode))
      )
    :config
    (progn
      (company-mode)
      (add-hook 'tcl-mode-hook
                (lambda ()
                  (company-mode)
                  (setq-local company-backends '(company-files
                                                 (company-dabbrev-code company-gtags company-etags company-keywords)
                                                 company-oddmuse company-dabbrev)))
                )
      )
    ))
(defun my-config/init-electric-spacing()
  "Initialize my package"
  (use-package electric-spacing
    :defer t
    :init
    ))
(defun my-config/init-ctags()
  (use-package ctags
    :defer t
    :init ))

(defun my-config/init-ctags-update()
  (use-package ctags-update
    :defer t
    :config
    (progn
      (autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on `ctags-auto-update-mode'." t)
      )
    :init ))
(defun my-config/init-plantuml-mode()
  (use-package plantuml-mode
    :defer t
    :init
    (progn
      (setq plantuml-jar-path (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      )
    :config
    (progn
      (setq plantuml-indent-regexp-end "^[ \t]*\\(?:@enduml\\|end\s+fork\\|fork\s+again\\|end\s+note\\|endif\\|end\\|elseif\\|else\\|endwhile\\|stop\\|}\\)")
      (setq plantuml-indent-regexp-start "^[ \t]*\\(?:@startuml\\|fork\s+again\\|fork\\|start\\|if\\|elseif\\|else\\|while\\|\\(?:.*\\)?\s*\\(?:[<>.*a-z-|]+\\)?\s*\\(?:\\[[a-zA-Z]+\\]\\)?\s+if\\|note\s+over\\|note\s+\\(\\(?:\\(?:buttom\\|left\\|right\\|top\\)\\)\\)\\(?:\s+of\\)?\\|.*{\\)")
      (defvar plantuml-indent-regexp-arrow "^[ \t]*\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
      (defvar plantuml-indent-regexp-arrow-1 "\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
      (defvar plantuml-indent-regexp-arrow-2 "^\s*.+\s+\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
      (defvar plantuml-indent-offset 3)

      (add-hook 'plantuml-mode-hook (lambda ()
                                      (set (make-local-variable 'indent-line-function)
                                           #'plantuml-indent-line)))
      (add-hook 'plantuml-mode-hook 'company-mode)
      (make-local-variable 'company-backends)
      (setq company-backends
            '( company-files
               (company-dabbrev-code company-gtags company-etags)
               company-abbrev
               ))
      (make-local-variable 'company-dabbrev-code-ignore-case)
      (setq company-dabbrev-code-ignore-case t)
      (make-local-variable 'company-dabbrev-code-everywhere)
      (setq company-dabbrev-code-everywhere t)

      )
    ))

(defun my-config/init-auctex()
  (use-package auctex
    :defer t
    :init (progn
            (load "auctex.el")
            (add-hook 'LaTeX-mode-hook (lambda()
                                         (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
                                         (setq TeX-command-default "XeLaTeX")
                                         (setq TeX-save-query  nil )
                                         (setq TeX-show-compilation t)
                                         )))
    ))
(defun my-config/init-auto-complete-auctex()
  (use-package auto-complete-auctex
    :defer t))

(defun my-config/init-tabbar ()
  (use-package tabbar
    :defer t
    ))
(defun my-config/init-tabbar-ruler ()
  (use-package tabbar-ruler
    :defer t
    :config
    (progn
      (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
      (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
      (setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
      (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
      (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
                                        ; scroll bar when your mouse is moving.
      )
    ))
(defun my-config/init-sos-mode()
  (use-package sos-mode
    :defer t
    :init
    (progn
      (autoload 'sos-mode "sos-mode.el" "some sos command." t)
      (sos-mode)
      )))
(defun my-config/post-init-org()
  (with-eval-after-load 'org
    (progn
      (setq org-confirm-babel-evaluate nil)
      ;; if emacs prompt that the launguages can't be evaluate and the setting of following is OK, please do spacemacs/recompile-elpa and try agin
      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (shell . t)
         (dot . t)
         (js . t)
         (latex .t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (awk . t)
         (ditaa . t)))
      (setq org-todo-keywords
           '((type "WORK(!)" "STUDY(!)" "|")
             (type "heyun(!)" "xulinliang(!)" "lina(!)" "chenshuxian(!)" "wangjunjie(!)" "huanqin(!)" "|" "DONE(!)")
             (sequence "TODO(t!)" "|" "DONE(d!)")
             (sequence "REPORT(r@/!)" "DEBUG(b@/!)" "KNOWNCAUSE(k@/!)" "|" "FIXED(f@/!)")
             (sequence "|" "CANCEL(c@/!)")
              ))
      (setq org-tag-alist '(
                            (:startgroup . nil)
                            ("product" . nil)
                            (:grouptags . nil)
                            ("develop" . ?d)
                            ("debug" . ?D)
                            (:endgroup . nil)

                            (:startgroup . nil)
                            ("DFE-members" . nil)
                            (:grouptags . nil)
                            ("heyun" . nil)
                            ("xulinliang" . nil)
                            ("lina" . nil)
                            ("chenshuxian" . nil)
                            ("wangjunjie" . nil)
                            ("huanqin" . nil)
                            (:endgroup . nil)
                            ))
      ))
  )
(defun my-config/init-compilation-mode()
  (use-package compilation-mode
    :defer t
    :init
    (progn
      (setq fm-error-regexp-emacs-alist
            '(
              (fm-WARNING
               "\\([^ \t\n,]+\\):\\([0-9]+\\):.*\[WARNING\]" 1 2 nil 2)
              (fm-ERROR
               "\\([^ \t\n,]+\\):\\([0-9]+\\):.*\[ERROR\]" 1 2 nil 2)
              ))
      (require 'compile)
      (my-config-error-regexp-add-emacs fm-error-regexp-emacs-alist))
      )
    )

(defun my-config/post-init-flycheck ()
  (dolist (mode '(verilog-mode))
    (spacemacs/enable-flycheck mode))
  (require 'flycheck)
  (flycheck-define-checker verilog-leda
    "A verilog coding style check by synopsys LEDA "
    :command ("leda" "+v2k" "-nobanner" "-nocompilemessage" "-nocode" "-work" temporary-file-name  "-log_dir" temporary-directory source)
    :error-patterns (
                     ;; (error line-start (file-name) ":" line ":" column ":" (1+ "a-zA-Z_") ":"  (message) line-end))
                     (error line-start (file-name) ":" line ":" (message) line-end))
    :modes verilog-mode
    )
  (flycheck-define-checker my-verilog-verilator
    "A Verilog syntax checker using the Verilator Verilog HDL simulator.

See URL `https://www.veripool.org/wiki/verilator'.
The original checker(verilog-verilator) doesn't work because of it chechouted that the verilator should be run by `start-process-shell-command',
for the reasion described above, use bash to start verilator
"
    :command ("sh" "verilator" "--lint-only" "-Wall" "-Wno-ASSIGNDLY" source)
    :error-patterns
    ((warning line-start "%Warning-" (zero-or-more not-newline) ": "
              (file-name) ":" line ": " (message) line-end)
     (error line-start "%Error: " (file-name) ":"
            line ": " (message) line-end))
    :modes verilog-mode)
  (flycheck-define-checker verilog-iverilog
    "A verilog syntax checker using icarus-verilog.

See URL `https://github.com/steveicarus/iverilog'"
    :command ("iverilog" "-tnull"  source)
    :error-patterns
    (
     (warning line-start (file-name) ":" line ": " "warning:" (message))
     (error line-start (file-name) ":" line ": " "error:" (message))
     )
    :modes verilog-mode
    )
  (flycheck-define-checker verilog-irun
    "A verilog syntax checker using irun

See URL `irun -helpall'"
    :command ("irun" "-nocopy" "-nclibdirname" temporary-directory  "-nolog" "-nohistory" "-compile"  source)
    :error-patterns
    (
     ;; (error line-start "ncvlog: *E," (id (1+ "A-Z")) "(" (file-name) "," line "|" column "):" (message))
     ;; ncvlog: *E,EXPLPA (SYN_CODE_GEN_tb.sv,25|14): expecting a left parenthesis ('(') [12.1.2][7.1(IEEE)]
     (error line-start "ncvlog: *E,"  (id (* (char "A-Z"))) " " "(" (file-name) "," line "|" (* (char "0-9")) ")" ":" (message))
     )
    :modes verilog-mode
    )
  (add-to-list 'flycheck-checkers 'verilog-leda)
  (add-to-list 'flycheck-checkers 'my-verilog-verilator)
  (add-to-list 'flycheck-checkers 'verilog-iverilog)
  (add-to-list 'flycheck-checkers 'verilog-irun)
  )

(defun my-config/init-company-verilog ()
  (use-package company-verilog
    :defer t
    :init
    (progn
      (autoload 'company-verilog "company-verilog" "company-mode in verilog-mode " t)
      (add-hook 'verilog-mode-hook 'company-verilog)
      ))
  )

(defun my-config/init-header2 ()
  (use-package header2
    :defer t
    :init
    )
  )
(defun my-config/init-flycheck-plantuml ()
  (use-package flycheck-plantuml
    :defer t
    :init
    )
  )
(defun my-config/init-company-english-helper ()
  (use-package company-english-helper
    :defer t
    :init
    (progn
      ;; (autoload 'company-english-helper "company-english-helper" "En-helper" t)
      (require 'company-english-helper)
      )
    ))

(defun my-config/init-awesome-tab ()
  (use-package awesom-tab
    :defer t
    :init
    (progn
      ;; (autoload 'company-english-helper "company-english-helper" "En-helper" t)
      (require 'awesome-tab)
      )
    ))

;;; packages.el ends here
