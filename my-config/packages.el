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
    graphviz-dot-mode
    blink-cursor-mode
    psvn
    (my-verilog :location local)
    ;;(mmm-mode :location local)
    (mmm-mode :location local)
    (highlight-symbol :location local)
    (tcl-dc-mode :location local)
    htmlize
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

(defun my-config/init-graphviz-dot-mode ()
  "Initialize my package"
  (use-package graphviz-dot-mode
    :defer t
    :init
    (progn
      (setq graphviz-dot-preview-extension "jpg")
      )))
(defun my-config/init-psvn ()
  "Initialize psvn"
  (use-package psvn
    :defer t
    :init
    ))

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
      (require 'my-verilog)
    )
    ))

(defun my-config/init-mmm-mode ()
  "Initialize my package"
  (use-package mmm-mode
    :defer t
    :init
    (progn
      (require 'mmm-mode)
      ;;(autoload 'mmm-mode "mmm-mode" "MMM mode" t)
      (setq mmm-global-mode 'maybe)
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
          :back "</script>")))
      )
    ))

(defun my-config/init-highlight-symbol ()
  "Initialize my package"
  (use-package highlight-symbol
    :defer t
    :init
    (progn
      (require 'highlight-symbol)
      ;;(autoload 'highlight-symbol "highlight-symbol" "Highlight-Func Mode" t)
      (global-set-key [(control f3)] 'highlight-symbol-at-point)
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
    ))


;;; packages.el ends here
