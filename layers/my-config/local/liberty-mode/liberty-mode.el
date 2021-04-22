(defvar liberty-imenu-generic-expression
  '(("*Library*"    "^\\s-*library\\s-*(\\([ \"a-zA-Z0-9_.:]+\\))" 0)
    ("*Cell*"  "^\\s-*cell\\s-*(\\([ \"A-Za-z0-9_]+\\))" 0)
    ("*wire_load*"  "^\\s-*wire_load\\s-*(\\([ \"A-Za-z0-9_]+\\))" 0)
    ("*operating_conditions*"  "^\\s-*operating_conditions\\s-*(\\([ \"A-Za-z0-9_]+\\))" 0)
    )
  "Imenu expression for Verilog mode.  See `imenu-generic-expression'.")

(defvar liberty-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Populate the syntax TABLE.
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?` "w" table)  ; ` is part of definition symbols in Verilog
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\' "." table)

    ;; Set up TABLE to handle block and line style comments.
    table)
  "Syntax table used in Liberty mode buffers.")

;;;###autoload
(define-derived-mode liberty-mode prog-mode "Liberty"
  "Major mode for editing synopsys liberty file."
  ;; :abbrev-table verilog-mode-abbrev-table
  ;; (set (make-local-variable 'beginning-of-defun-function)
  ;;      'verilog-beg-of-defun)
  ;; (set (make-local-variable 'end-of-defun-function)
  ;;      'verilog-end-of-defun)
  (set-syntax-table liberty-mode-syntax-table)
  ;; (set (make-local-variable 'indent-line-function)
  ;;      #'verilog-indent-line-relative)
  ;; (set (make-local-variable 'comment-indent-function) 'verilog-comment-indent)
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end) "*/")
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|// *")
  (set (make-local-variable 'comment-multi-line) nil)
  ;; Set up for compilation
  ;; (when (boundp 'hack-local-variables-hook)  ; Also modify any file-local-variables
  ;;   (add-hook 'hack-local-variables-hook 'verilog-modify-compile-command t))

  ;; Stuff for GNU Emacs
  ;; (set (make-local-variable 'font-lock-defaults)
  ;;      `((verilog-font-lock-keywords
  ;;         verilog-font-lock-keywords-1
  ;;         verilog-font-lock-keywords-2
  ;;         verilog-font-lock-keywords-3)
  ;;        nil nil nil
  ;;        ,(if (functionp 'syntax-ppss)
  ;;             ;; verilog-beg-of-defun uses syntax-ppss, and syntax-ppss uses
  ;;             ;; font-lock-beginning-of-syntax-function, so
  ;;             ;; font-lock-beginning-of-syntax-function, can't use
  ;;             ;; verilog-beg-of-defun.
  ;;             nil
  ;;           'verilog-beg-of-defun)))

  ;; Stuff for multiline font-lock
  (set (make-local-variable 'font-lock-multiline) t)

    ;; Tell imenu how to handle Verilog.
  (set (make-local-variable 'imenu-generic-expression)
       liberty-imenu-generic-expression)
  ;; Tell which-func-modes that imenu knows about verilog
  (when (and (boundp 'which-func-modes) (listp which-func-modes))
    (add-to-list 'which-func-modes 'verilog-mode))
  ;; hideshow support
  ;; (when (boundp 'hs-special-modes-alist)
  ;;   (unless (assq 'verilog-mode hs-special-modes-alist)
  ;;     (setq hs-special-modes-alist
  ;;           (cons '(verilog-mode
  ;;                   "\\<begin\\>\\|\\<task\\>\\|\\<function\\>\\|\\<class\\>\\|\\<interface\\>\\|\\<fork\\>\\|(\\|\\<`ifdef\\>\\|\\<`ifndef\\>"
  ;;                   "\\<end\\>\\|\\<endtask\\>\\|\\<endfunction\\>\\|\\<endclass\\>\\|\\<endinterface\\>\\|\\<join\\>\\|)\\|\\<`endif\\>"
  ;;                   nil
  ;;                   verilog-forward-sexp-function)
  ;;                 hs-special-modes-alist))
  ;;     ))

  ;; (add-hook 'completion-at-point-functions
  ;;           #'verilog-completion-at-point nil 'local)

  ;; Stuff for autos
  ;; (add-hook (if (boundp 'write-contents-hooks) 'write-contents-hooks
  ;;             'write-contents-functions) ; Emacs >= 22.1
  ;;           'verilog-auto-save-check nil 'local)
  ;; verilog-mode-hook call added by define-derived-mode
  )

(provide 'liberty-mode)
