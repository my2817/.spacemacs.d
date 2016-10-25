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
    graphviz-dot-mode
    blink-cursor-mode
    psvn
    (my-verilog :location local)
    ;;(mmm-mode :location local)
    mmm-mode
    highlight-symbol
    (tcl-dc-mode :location local)
    htmlize
    electric-spacing
    ctags
    ctags-update
    auctex
    auto-complete-auctex
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
      (global-auto-complete-mode 1)
      (add-to-list 'ac-modes 'graphviz-dot-mode)
      (add-to-list 'ac-modes 'makefile-gmake-mode)
      (add-to-list 'ac-modes 'TeX-latex-mode)
      (add-to-list 'ac-dictionary-directories (concat
                                               (or (file-name-directory #$) (car load-path))
                                               "local/ac-dict/"))
      (add-to-list 'ac-sources 'ac-source-filename)
      (add-to-list 'ac-sources 'ac-source-abbrev)
      ;;(add-to-list 'ac-dictionary-directories (expand-file-name "local/ac-dict/" user-emacs-directory))
      (setq-default ac-disable-faces nil))))

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
;;; packages.el ends here
