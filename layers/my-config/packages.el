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
    ;;blink-cursor-mode
    ;; psvn
    (my-verilog :location local)
    verilog-mode
    ;;(mmm-mode :location local)
    mmm-mode
    highlight-symbol
    (tcl-dc-mode :location local)
    ;;htmlize
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
    ;; org-appear
    (compilation-mode :location built-in)
    flycheck
    flycheck-plantuml
    (company-verilog :location local)
    (company-english-helper :location local)
    (awesome-tab :location (recipe
                            :fetcher github
                            :repo manateelazycat/awesome-tab)) ;;from [https://github.com/manateelazycat/awesome-tab]
    ;; header2
    (awesome-pair :location (recipe
                             :fetcher github
                             :repo manateelazycat/awesome-pair))
    pyim-wbdict
    pyim
    posframe
    company-posframe
    cnfonts
    beacon
    company-tabnine
    (thing-edit :location (recipe
                           :fetcher github
                           :repo manateelazycat/thing-edit))
    ;; (thing-edit :location local)
    ;; (aweshell :location (recipe
    ;;                      :fetcher github
    ;;                      :repo manateelazycat/aweshell))
    ;; (eaf :location local)
    ;; magit-todos
    ;; (magit-gerrit :location (recipe
    ;;                          :fetcher github
    ;;                          :repo zeph1e/magit-gerrit)) ;; https://github.com/terranpro/magit-gerrit/pull/56

    separedit 
    ;;    (snails :location (recipe
    ;;                         :fetcher github
    ;;                         :repo manateelazycat/snails))
    ;; (rime :location (recipe
    ;;                  :fetcher github
    ;;                  :repo DogLooksGood/emacs-rime))

    (vc-msg :location (recipe
                       :fetcher github
                       :repo redguardtoo/vc-msg))
    (flywrap :location local)
    (avy-thing-edit :location (recipe
                               :fetcher github
                               :repo lyjdwh/avy-thing-edit))
    chinese-word-at-point
    bing-dict
    google-translate
    youdao-dictionary
    sdcv
    (multi-translate :location (recipe
                                :fetcher github
                                :repo twlz0ne/multi-translate.el))
    ;; org-roam
    ;; org-roam-server
    ;; mermaid-mode
    ;; (hideshowvis :location local)
    atomic-chrome
    (liberty-mode :location local)
    citre
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
      (add-hook 'verilog-mode-hook 'my-verilog)
      (autoload 'my-verilog "my-verilog" "my configuration of verilog-mode " t)
      )))

(defun my-config/init-verilog-mode ()
  (use-package verilog-mode
    :defer t
    :config
    ;; (modify-syntax-entry ?` "." verilog-mode-syntax-table)
    ;; `electric-verilog-tab' error while excute `verilog-calculate-indent'
    ;;  because "`" is not a word-constituent from the last line, we need to move "`" outside of "\\<" in verilog-directive-re-1
    ;; (defconst verilog-directive-re-1 "[ 	]*`\\<\\(\\(?:__\\(?:FILE__\\|LINE\\)\\|begin_keywords\\|c\\(?:\\(?:as\\|elldefin\\)e\\)\\|def\\(?:ault\\(?:_nettype\\)?\\|ine\\)\\|e\\(?:ls\\(?:e\\|if\\)\\|nd\\(?:_keywords\\|celldefine\\|for\\|if\\|protect\\|switch\\|while\\)\\)\\|for\\(?:mat\\)?\\|i\\(?:f\\(?:n?def\\)?\\|nclude\\)\\|l\\(?:et\\|ine\\)\\|nounconnected_drive\\|pr\\(?:agma\\|otect\\)\\|resetall\\|switch\\|time\\(?:_?scale\\)\\|u\\(?:n\\(?:connected_drive\\|def\\(?:ineall\\)?\\)\\|selib\\)\\|while\\)\\)\\>")
    (remove-hook 'completion-at-point-functions
                 'verilog-completion-at-point t)

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
      (setq plantuml-indent-regexp-end "^[ \t]*\\(?:@enduml\\|end\s+fork\\|fork\s+again\\|end\s+note\\|endif\\|end\\|elseif\\|else\\|endwhile\\|repeat\s+while\\|stop\\|}\\)")
      (setq plantuml-indent-regexp-start "^[ \t]*\\(?:@startuml\\|fork\s+again\\|fork\\|start\\|if\\|elseif\\|else\\|while\\|loop\\|alt\\|repeat$\\|\\(?:.*\\)?\s*\\(?:[<>.*a-z-|]+\\)?\s*\\(?:\\[[a-zA-Z]+\\]\\)?\s+if\\|note\s+over\\|note\s+\\(\\(?:\\(?:buttom\\|left\\|right\\|top\\)\\)\\)\\(?:\s+of\\)?\\|.*{\\)")
      (defvar plantuml-indent-regexp-arrow "^[ \t]*\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
      (defvar plantuml-indent-regexp-arrow-1 "\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
      (defvar plantuml-indent-regexp-arrow-2 "^\s*.+\s+\\(?:\\(?:<\\|<|\\|o\\|\\*\\)\\(?:\\.\\|-\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:\\.\\|-\\)\\|\\(?:-\\|\\.\\)\\(?:down\\|up\\|left\\|right\\)?\\(?:-\\|\\.\\)\\(?:>\\||>\\|\\*\\|o\\)\\)")
      (defvar plantuml-indent-offset 3)

      (add-hook 'plantuml-mode-hook (lambda ()
                                      (set (make-local-variable 'indent-line-function)
                                           #'plantuml-indent-line)))
      (add-hook 'plantuml-mode-hook 'company-mode)
      (add-hook 'plantuml-mode-hook 'electric-spacing-mode)
      (add-hook 'plantuml-mode-hook (lambda ()
                                      (set (make-local-variable 'pyim-english-input-switch-functions)
                                           '(;;pyim-probe-dynamic-english
                                             pyim-probe-isearch-mode
                                             ;;pyim-probe-program-mode
                                             pyim-probe-org-speed-commands
                                             pyim-probe-org-structure-template))))
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

      (setq plantuml-default-exec-mode 'jar)

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
      (autoload 'my-verilog-get-last-history-log "my-verilog" "get change log of today" t)
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
         (latex . t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (awk . t)
         (ditaa . t)
         ;; (mermaid . t)
         ))

      (setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc")
      (setq org-todo-keywords
            '((type "WORK(!)" "STUDY(!)" "|")
              (sequence "TODO(t!)" "ONGOING(o!)" "|" "DONE(d!)")
              ;; debug sequence
              (sequence "REPORT(r@/!)" "DEBUG(b@/!)" "KNOWNCAUSE(k@/!)" "|" "FIXED(f@/!)" "NOT_FIX(@/!)")
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
                            ("DFE" . nil)
                            ("DV" . nil)
                            ("DBE" . nil)
                            (:grouptags . nil)
                            ("HeYun" . ?h)
                            ("YuYisong" . ?y)
                            ("WangJunjie" . ?w)
                            ("JiangSongzuo" . ?j)
                            ("HuangQin" . ?H)
                            ("LiuWanhang" . ?l)
                            ("SunYahui" . ?s)
                            ("BaiBo" . ?b)
                            ("ShuXin" . ?S)
                            ("LanTing" . ?L)
                            ("ZhuoKang" . ?z)
                            ("ZhangXing" . ?Z)
                            ("ZhangJianguo" . ?J)
                            ("FengYang" . ?f)
                            (:endgroup . nil)
                            ))
      ;; 让中文也可以不加空格就使用行内格式
      (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
      (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
      (org-element-update-syntax)
      ;; 规定上下标必须加 {}，否则中文使用下划线时它会以为是两个连着的下标
      (setq org-use-sub-superscripts "{}")

      (require 'org-crypt)
      ;; 當被加密的部份要存入硬碟時，自動加密回去
      (org-crypt-use-before-save-magic)
      ;; 設定要加密的 tag 標籤為 secret
      (setq org-crypt-tag-matcher "secret")
      ;; 避免 secret 這個 tag 被子項目繼承 造成重複加密
      ;; (但是子項目還是會被加密喔)
      (setq org-tags-exclude-from-inheritance (quote ("secret")))
      ;; 用於加密的 GPG 金鑰
      ;; 可以設定任何 ID 或是設成 nil 來使用對稱式加密 (symmetric encryption)
      (setq org-crypt-key nil)
      ;; (custom-set-variables '(org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ" "Ⅺ" "Ⅻ" "Ⅼ" "Ⅽ" "Ⅾ" "Ⅿ" )))
      (custom-set-variables '(org-superstar-headline-bullets-list '("○" "✿" "✸" "◉")))
      ;; after org9.2 [[https://orgmode.org/Changes.html][changlog]]
      ;; prefer using previous patterns, e.g. <s, goto org-tempo.el for more information
      (add-to-list 'org-modules 'org-tempo)
      ;; export into freemind *.mm file, and open with freemplane
      ;; to support chinese in freeplane, should do the following
      ;; - linux
      ;;   export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dsun.java2d.xrender=true -Dfile.encoding=UTF8 $_JAVA_OPTIONS"
      ;; - window 需要找到Freeplane的安装路径（我的为D:\Program Files\Freeplane），将其加入到启动配置文件freeplane.l4j.ini中
      ;;   #+begin_example
      ;;     -Dawt.useSystemAAFontSettings=on
      ;;     -Dswing.aatext=true
      ;;     -Dsun.java2d.xrender=true
      ;;     -Dfile.encoding=UTF8
      ;;   #+end_example
      ;;
      (add-to-list 'org-modules 'ox-freemind)
      (setq org-hide-emphasis-markers t)
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
  (flycheck-define-checker verilog-verilator
    "A Verilog syntax checker using the Verilator Verilog HDL simulator.

See URL `https://www.veripool.org/wiki/verilator'.
The original checker(verilog-verilator) doesn't work because of it chechouted that the verilator should be run by `start-process-shell-command',
for the reasion described above, use bash to start verilator
"
    :command ("verilator_bin" "--lint-only" "-Wall" "-Wno-ASSIGNDLY" source)
    :error-patterns
    ((warning line-start "%Warning-" (zero-or-more not-newline) ": "
              (file-name) ":" line ": " (message) line-end)
     (error line-start "%Error: " (file-name) ":"
            line ": " (message) line-end)
     (error line-start "%Error: " (message)))
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
     (error line-start "irun: *E,"  (id (* (char "A-Z"))) ": "  (message))
     )
    :modes verilog-mode
    )
  (add-to-list 'flycheck-checkers 'verilog-leda)
  (add-to-list 'flycheck-checkers 'verilog-verilator)
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
      (toggle-company-english-helper)
      )
    ))

(defun my-config/init-awesome-tab ()
  (use-package awesome-tab
    :init
    (progn
      ;; need all-the-icon fonts: https://github.com/domtronn/all-the-icons.el/tree/master/fonts
      (require 'awesome-tab)
      ;; (awesome-tab-mode)
      )
    :config
    (custom-set-variables '(awesome-tab-height 120))
    ))

(defun my-config/init-awesome-pair()
  (use-package awesome-pair
    :defer t
    :init (require 'awesome-pair)
    :config (progn
              (dolist (hook (list
                             'c-mode-common-hook
                             'c-mode-hook
                             'c++-mode-hook
                             'java-mode-hook
                             'haskell-mode-hook
                             'emacs-lisp-mode-hook
                             'lisp-interaction-mode-hook
                             'lisp-mode-hook
                             'maxima-mode-hook
                             'ielm-mode-hook
                             'sh-mode-hook
                             'makefile-gmake-mode-hook
                             'php-mode-hook
                             'python-mode-hook
                             'js-mode-hook
                             'go-mode-hook
                             'qml-mode-hook
                             'jade-mode-hook
                             'css-mode-hook
                             'ruby-mode-hook
                             'coffee-mode-hook
                             'rust-mode-hook
                             'qmake-mode-hook
                             'lua-mode-hook
                             'swift-mode-hook
                             'minibuffer-inactive-mode-hook
                             'verilog-mode-hook
                             ))
                (add-hook hook '(lambda () (awesome-pair-mode 1)))

                ;; the following keybindings is conflict with `electric-spacing-mode
                ;; (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
                ;; (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
                ;; (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
                ;; (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
                ;; (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
                ;; (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
                ;; (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

                ;; (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
                ;; (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

                ;; (define-key awesome-pair-mode-map (kbd "M-o") 'awesome-pair-backward-delete)
                ;; (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
                ;; (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

                ;; (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
                ;; (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
                ;; (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
                ;; (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
                ;; (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

                ;; (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-right)
                ;; (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-left)
                ;; (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline)
                )
              )))

(defun my-config/init-pyim ()
  (use-package pyim
    :defer nil
    :demand t
    :ensure nil
    :init
    (progn
      (require 'pyim)
      )
    :bind
    (("C-c M-j" . pyim-convert-string-at-point) ;与 pyim-probe-dynamic-english 配合
     )
    :config
    (defun pyim-add-unread-command-events (key &optional reset)
      " \"no record\" in mini buffer"
      (when reset
        (setq unread-command-events nil))
      (setq unread-command-events
            (if (characterp key)
                (cons key unread-command-events)
              (append (mapcan (lambda (e) (list e))
                              (append key nil))
                      unread-command-events))))
    (progn
      (setq default-input-method "pyim")
      ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
      ;; 我自己使用的中英文动态切换规则是：
      ;; 1. 光标只有在注释里面时，才可以输入中文。
      ;; 2. 光标前是汉字字符时，才能输入中文。
      ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
      (setq-default pyim-english-input-switch-functions
                    '(pyim-probe-dynamic-english
                      pyim-probe-isearch-mode
                      pyim-probe-program-mode
                      pyim-probe-org-speed-commands
                      pyim-probe-org-structure-template))

      (setq-default pyim-punctuation-half-width-functions
                    '(pyim-probe-punctuation-line-beginning
                      pyim-probe-punctuation-after-punctuation))
      ;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
      ;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
      ;; 手动安装 posframe 包。
      ;; (setq pyim-page-tooltip 'popup)
      (setq pyim-page-tooltip 'posframe)
      ;; 选词框显示5个候选词
      (setq pyim-page-length 5)

      ;; (setq pyim-default-scheme 'wubi)
      (setq pyim-default-scheme 'xiaohe-shuangpin)
      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(:name "pyim-bigdict" :file  "~/.spacemacs.d/layers/my-config/local/pyim-dict/pyim-bigdict.pyim" :elpa t )))
      )))

(defun my-config/init-pyim-wbdict ()
  (use-package pyim-wbdict
    :defer t
    :after pyim
    :init
    (pyim-wbdict-v98-enable)
    :config
    (progn
      (setq pyim-assistant-scheme-enable t)
      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(:name "electric-wbdict" :file  "~/.spacemacs.d/layers/my-config/local/pyim-dict/electric-wbdict.pyim" :elpa t))
        (message "electric-wbdict 没有安装，pyim-wbdict 启用失败。"))

      (if (featurep 'pyim)
          (pyim-extra-dicts-add-dict
           `(:name "wbdict-use" :file ,"~/.spacemacs.d/layers/my-config/local/pyim-dict/pyim-wbdict-user.pyim" :elpa t))
        (message "wbdict-user 没有安装，pyim-wbdict 启用失败。"))
      )
    )
  )

(defun my-config/init-posframe ()

  (use-package posframe
    :defer t
    :init
    )
  )
(defun my-config/init-company-posframe ()
  (use-package company-posframe
    :if (display-graphic-p)
    :after company
    :defer t
    :init
    :config
    )
  )

(defun my-config/init-cnfonts ()
  (use-package cnfonts
    :defer t
    :init
    (progn
      (cnfonts-enable)
      (cnfonts-set-spacemacs-fallback-fonts))
    ))
(defun my-config/init-beacon ()
  (use-package beacon
    :defer t
    :init
    (progn
      ;; (beacon-mode)
      )))

(defun my-config/init-company-tabnine ()
  (use-package company-tabnine
    :defer t
    :init
    ;; (progn
    ;;   (require 'company-tabnine))
    )
  )
;;; 配置company-tabnine作为company的后端
(defun tabnine/post-init-company-tabnine()
  (with-eval-after-load 'company
    (add-to-list 'company-backends #'company-tabnine)
    )
  )


(defun my-config/init-thing-edit ()
  (use-package thing-edit
    :defer t
    :ensure t
    :init
    (progn
      (require 'thing-edit)
      )))

(defun my-config/init-avy-thing-edit ()
  (use-package avy-thing-edit
    :defer t
    :init
    (progn
      (require 'avy-thing-edit)
      )))

(defun my-config/init-aweshell ()
  (use-package aweshell
    :defer t
    :init
    (progn
      (require 'aweshell)
      )))

(defun my-config/init-magit-todos ()
  (use-package magit-todos
    :defer t
    :init
    (progn
      (setq magit-todos-nice (if (executable-find "nice") t nil))
      (magit-todos-mode 1)
      ;;performance, this will check todos in all branches
      (setq magit-todos-branch-list nil)
      )))
(defun my-config/init-magit-gerrit ()
  (use-package magit-gerrit
    :defer t
    :init
    (progn
      ;; (setq-default magit-gerrit-ssh-creds "myid@gerrithost.org")
      ;; DONT compile magit-gerrit.el into elc file(remove it if)
      (require 'magit-gerrit)
      )))

(defun my-config/init-separedit ()
  (use-package separedit
    :init
    (progn
      (require 'separedit)
      (define-key prog-mode-map (kbd "C-c '") #'separedit)
      (setq separedit-default-mode 'org-mode))
    :config
    (progn
      ;; add verilog-mode into separedit
      (setq separedit-code-lang-modes
            '(("C" . c-mode)
              ("C++" . c++-mode)
              ("asymptote" . asy-mode)
              ("bash" . sh-mode)
              ("calc" . fundamental-mode)
              ("cpp" . c++-mode)
              ("ditaa" . artist-mode)
              ("dot" . fundamental-mode)
              ("elisp" emacs-lisp-mode lisp-interaction-mode)
              ("ocaml" . tuareg-mode)
              ("screen" . shell-script-mode)
              ("shell" . sh-mode)
              ("sqlite" . sql-mode)
              ("verilog" . verilog-mode)))

      (setq separedit-comment-delimiter-alist
            '((("//+" "\\*+")
               c-mode c++-mode csharp-mode css-mode go-mode java-mode js-mode objc-mode php-mode swift-mode verilog-mode)
              (("//+!" "//+" "\\*+")
               . rust-mode)
              (("--")
               applescript-mode haskell-mode lua-mode)
              (("//+")
               pascal-mode fsharp-mode)
              ((";+")
               emacs-lisp-mode lisp-interaction-mode common-lisp racket-mode scheme-mode)
              (("#+")
               python-mode ruby-mode)))
      (setq separedit-comment-encloser-alist
            '((("/\\*+" "\\*/")
               c-mode c++-mode csharp-mode css-mode go-mode java-mode js-mode objc-mode php-mode rust-mode swift-mode verilog-mode)
              (("{-" "-}")
               . haskell-mode)
              (("{" "}")
               . pascal-mode)
              (("(\\*" "\\*)")
               applescript-mode fsharp-mode ocaml-mode)
              (("#|" "#|")
               common-lisp racket-mode scheme-mode)
              (("<!--" "-->")
               html-mode xml-mode)
              (("--\\[[" "--\\]\\]")
               . lua-mode)
              (("=begin" "=end")
               . ruby-mode)))
      )))

(defun my-config/init-snails ()
  (use-package snails
    :defer t
    :init
    (progn
      (require 'snails)
      )))

(defun my-config/init-rime ()
  (use-package rime
    :defer t
    :init
    (require 'rime)
    :config
    (progn
      (setq rime-user-data-dir "~/.config/fcitx/rime")
      (setq rime--module-path "~/.emacs.d/.cache/quelpa/build/rime/librime-emacs.so")

      (setq rime-posframe-properties
            (list :background-color "#333333"
                  :foreground-color "#dcdccc"
                  :font "WenQuanYi Micro Hei Mono-14"
                  :internal-border-width 10))

      ;; (setq default-input-method "rime"
      ;;       rime-show-candidate 'posframe)

      (setq rime-disable-predicates
            '(rime-predicate-evil-mode-p
              rime-predicate-after-alphabet-char-p
              rime-predicate-prog-in-code-p
              rime-predicate-auto-english-p))
      )))

(defun my-config/init-vc-msg ()
  (use-package vc-msg
    :defer t
    ))

(defun my-config/init-flywrap ()
  (use-package flywrap
    :defer t
    :init
    (progn
      (require 'flywrap)
      ))
  )

(defun my-config/init-chinese-word-at-point ()
  (use-package chinese-word-at-point
    :defer t
    ))

(defun my-config/init-bing-dict ()
  (use-package bing-dict
    :defer t
    ))

(defun my-config/init-google-translate ()
  (use-package google-translate
    :defer t
    ))

(defun my-config/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer t
    ))

(defun my-config/init-sdcv ()
       (use-package sdcv
         :defer t
         ))

(defun my-config/init-multi-translate ()
  (use-package multi-translate
    :defer t
    ))

(defun my-config/init-org-roam()
  (use-package org-roam
    :defer t
    :config
    (progn (setq org-roam-directory "/mnt/c/Users/User/Documents/gtd"))
    :init
    (progn (org-roam-mode))
    ))

(defun my-config/init-org-roam-server()
  (use-package org-roam-server
    :defer t
    :init
    (progn (org-roam-server-mode))
    ))

(defun my-config/init-eaf()
  "wsl:
git clone eaf into ./local/eaf
apt install -y pyqt5 pyqt5-dev
pip install PyQt5 PyQtWebEngine

shell:
eval $(dbus-launch)
export DBUS_SESSION_BUS_ADDRESS
emacs

win10:
git clone eaf
install python3
install pip
install node
win10菜单搜索【管理应用执行别名】关闭下面两个 python/python3的选项
PowerShell: cd to eaf
node install-eaf-win32.js

"
  (use-package eaf
    :load-path "~/.spacemacs.d/layers/my-config/local/eaf" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
    :init
    (use-package epc :defer t :ensure t)
    (use-package ctable :defer t :ensure t)
    (use-package deferred :defer t :ensure t)
    (use-package s :defer t :ensure t)
    :custom
    (eaf-browser-continue-where-left-off t)
    :config
    ;; (eaf-setq eaf-browser-enable-adblocker "true")
    ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
    ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
    ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
    ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
    )
  )

(defun my-config/init-mermaid-mode()
  (use-package mermaid-mode
    :defer t
    :config
    (setq ob-mermaid-cli-path "~/node_modules/.bin/mmdc")
    ))

(defun my-config/init-hideshowvis()
  (use-package hideshowvis
    :defer t
    :init
    (autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")
    (autoload 'hideshowvis-minor-mode
      "hideshowvis"
      "Will indicate regions foldable with hideshow in the fringe."
      'interactive)
    (dolist (hook (list 'emacs-lisp-mode-hook
                        'c++-mode-hook
                        'verilog-mode-hook
                        ))
      (add-hook hook 'hideshowvis-enable))
    ))

(defun my-config/init-atomic-chrome()
  ;; 浏览器文本编辑区域调用外部编辑器
  ;; firefox extension: GhostText
  (use-package atomic-chrome
    :defer t
    :config
    (atomic-chrome-start-server)
    ))

(defun my-config/init-org-appear()
  (use-package org-appear
    :defer t
    :init
    (add-hook 'org-mode-hook 'org-appear-mode)
    :config
    (setq org-appear-autolinks t)
    ))

(defun my-config/init-liberty-mode()
  (use-package liberty-mode
    :defer t
    :init
    (progn
      (autoload 'liberty-mode "liberty-mode.el" "Edite Synopsys liberty library file." t)
      ;; (sos-mode)
      )
    :config
    ))

(defun my-config/init-citre()
  (use-package citre
    :init
    (require 'citre)
    (require 'citre-config)
    :defer t
    :config
    ;;;;;
    ;; the folloing code has been implemented by citre
    ;; (defun my-verilog-get-symbol-atpt ()
    ;;   (let* ((sym (symbol-at-point))
    ;;          (char (substring (symbol-name sym) 0 1))
    ;;          )
    ;;     (if (string= "`" char)
    ;;         (substring (symbol-name sym) 1)
    ;;       (symbol-name sym))
    ;;     )
    ;;   )
    ;; (defvar citre-lang-verilog-plist
    ;;   `(:get-symbol
    ;;     my-verilog-get-symbol-atpt
    ;;     )
    ;;   "Verilog language support for Citre.")
    ;; (setf (alist-get 'verilog-mode citre-language-support-alist)
    ;;       citre-lang-verilog-plist)
    (defun citre-jump+ ()
      (interactive)
      (condition-case _
          (citre-jump)
        (error (call-interactively #'xref-find-definitions))))
    ))

;;; packages.el ends here
