;;; my-verilog-extras.el


;;; Enhancements to verilog-mode

(require 'verilog-mode)
(unless (fboundp 'hs-special-modes-alist)
  (defvar hs-special-modes-alist nil))
(add-hook 'verilog-mode-hook 'hs-minor-mode)
(add-to-list 'hs-special-modes-alist '(verilog-mode  "\\<begin\\>\\|\\<task\\>\\|\\<function\\>\\|\\<class\\>\\|\\<interface\\>\\|\\<fork\\>\\|("
                                                     "\\<end\\>\\|\\<endtask\\>\\|\\<endfunction\\>\\|\\<endclass\\>\\|\\<endinterface\\>\\|\\<join\\>\\|)"
                                                     nil  verilog-forward-sexp-function))

;;; Port copy/paste

(require 'align)

(defvar my-verilog-inst-name nil)
(defvar my-verilog-inst-ports nil)

(defun my-verilog-get-module-name ()
  "Get module name"
  (interactive)
  (save-excursion
    (when (verilog-re-search-backward "\\bmodule\\b" nil t)
      (skip-syntax-forward "w_")
      (when (verilog-re-search-forward "[a-zA-Z][a-zA-Z0-9_]*" nil t)
        (match-string-no-properties 0)))))

(defun my-verilog-port-copy ()
  "Copy a module's ports when in it's definition."
  (interactive)
  (setq my-verilog-inst-name (my-verilog-get-module-name))
  (setq my-verilog-inst-ports (verilog-read-decls)))

(defun my-verilog-insert-port-list (port-list port-comment)
  (dolist (port port-list)
    (insert (concat "." (car port) "(" (car port) ")," port-comment))
    (verilog-indent-line-relative)
    (verilog-indent-line-relative)
    (end-of-line)
    (insert "\n")))

(defun my-verilog-get-instance-name ()
  "Prompt for an instance name."
  (let ((name (concat "i" (upcase my-verilog-inst-name)))
        result)
    (setq result (read-from-minibuffer (format "Instance name (default `%s'): " name)))
    (if (string= result "") name result)))

(defun my-verilog-port-paste-inst ()
  "Paste an instance of a copied module."
  (interactive)
  (when (and my-verilog-inst-name my-verilog-inst-ports)
    (save-excursion
      (let (beg end rule)
        (insert my-verilog-inst-name " " (my-verilog-get-instance-name) "(")
        (verilog-indent-line-relative)
        (verilog-indent-line-relative)
        (end-of-line)
        (setq beg (point))
        (my-verilog-insert-port-list (verilog-decls-get-inputs my-verilog-inst-ports) " // input")
        (my-verilog-insert-port-list (verilog-decls-get-inouts my-verilog-inst-ports) " // inout")
        (my-verilog-insert-port-list (verilog-decls-get-outputs my-verilog-inst-ports) " // output")
        (save-excursion (when (re-search-backward "," beg t)
                          (replace-match " ")))
        (insert ");")
        (beginning-of-line)
        (verilog-indent-line-relative)
        (end-of-line)
        (setq end (point))
        (setq rule (list
                    (list nil
                          (cons 'regexp "\\(\\s-*\\)(")
                          (cons 'group 1))))
        (align-region beg end 'entire rule nil nil)
        (setq end (point))
        (setq rule (list
                    (list nil
                          (cons 'regexp "\\(\\s-*\\)//")
                          (cons 'group 1))))
        (align-region beg end 'entire rule nil nil)))))

(defun my-verilog-port-paste-wires ()
  "Paste wires of a copied module."
  (interactive)
  (when my-verilog-inst-ports
    (save-excursion
      (let ((beg (point)) end)
        (verilog-insert-definition (verilog-decls-get-inputs my-verilog-inst-ports) "wire" 0 nil)
        (verilog-insert-definition (verilog-decls-get-inouts my-verilog-inst-ports) "wire" 0 nil)
        (verilog-insert-definition (verilog-decls-get-outputs my-verilog-inst-ports) "wire" 0 nil)
        (align beg (point))))))

;;; Imenu

(if (string-match "XEmacs" emacs-version)
    (fset 'verilog-match-string 'match-string)
  (fset 'verilog-match-string 'match-string-no-properties))

(defvar verilog-imenu-flatten t
  "*Non-nil means flatten the heirarchical imenu output.")

(defvar verilog-imenu-show-instance-type t
  "*Non-nil means show the instance type with the instance name.")

(defvar verilog-imenu-qualify-names nil
  "*Non-nil means qualify names with the module they are in.")

(defun verilog-trim-trailing-whitespace (string)
  (if (string-match "XEmacs" emacs-version)
      (replace-in-string string "[ \t\n]+$" "")
    (replace-regexp-in-string "[ \t\n]+$" "" string)))

(defun verilog-sort-alist-by-car-string (alist)
  (sort alist '(lambda (x y) (string< (car x) (car y)))))

(defun verilog-imenu-create-add-item-alist (name item-alist final-alist)
  (when item-alist
    (push (imenu--split-menu (verilog-sort-alist-by-car-string item-alist) name) final-alist))
  final-alist)

(defun verilog-imenu-create-find-instances-or-modports (end)
  (save-excursion
    (let ((instance-alist '()))
      (while (re-search-forward
              "^\\s-*\\([a-zA-Z0-9_]+\\)\\([ \t\n]+#(.*)\\)?[ \t\n]+\\([a-zA-Z0-9_]+\\)[ \t\n]*("
              end t)
        (condition-case nil
            (let ((instance-type (verilog-match-string 1)) (instance-name (verilog-match-string 3))
                  (instance-pos (match-beginning 0)))
              (backward-char)
              (forward-sexp)
              (when (looking-at "[ \t\n]*;")
                (if (string= instance-type "modport")
                    (push (cons instance-name instance-pos) instance-alist)
                  (if verilog-imenu-show-instance-type
                      (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
                    (push (cons instance-name instance-pos) instance-alist)))))
          (error nil)))
      instance-alist)))

(defun verilog-imenu-create-find-data-types (data-type end)
  (save-excursion
    (let ((type-alist '()))
      (while (re-search-forward (concat "^\\s-*\\(typedef\\s-+\\)?\\(" data-type "\\)\\s-*\\([^{;]*?\\)\\s-*\\([{;]\\)") end t)
        (condition-case nil
            (let ((type-id (verilog-match-string 3)) (type-terminator (verilog-match-string 4))
                  (type-pos (match-beginning 0)))
              (if (string= type-terminator "{")
                  (progn (backward-char)
                         (forward-sexp)
                         (re-search-forward "\\([a-zA-Z0-9_]+\\)[ \t\n]*;" end t)
                         (push (cons (verilog-match-string 1) type-pos) type-alist))
                (push (cons (verilog-trim-trailing-whitespace type-id) type-pos) type-alist)))
          (error nil)))
      type-alist)))

(defun verilog-imenu-create-parse-entity ()
  (when (re-search-forward "^\\s-*\\(module\\|interface\\|package\\|class\\|function\\|task\\)[ \t\n]+\\([a-zA-Z0-9_:]+\\)" nil t)
    (let ((entity-type (verilog-match-string 1)) (entity-name
                                                  (if (string= (verilog-match-string 1) "function")
                                                      (progn
                                                        (save-excursion
                                                          (beginning-of-line)
                                                          (re-search-forward "^\\s-*function[ \t]+\\([a-z]+\\s-+\\)?\\([a-zA-Z0-9_:]+\\)\\s-*[(;]" nil t)
                                                          (setq entity-name (verilog-match-string 2))
                                                          ))
                                                    (verilog-match-string 2)))
          (entity-start (match-beginning 0)) (entity-end) (end 0) (final-alist '())
          (nested-entity) (found-routine) (routine-type)
          (instance-alist '()) (modport-alist '()) (module-alist '()) (interface-alist '()) (package-alist '())
          (enum-alist '()) (struct-alist '()) (union-alist '()) (function-alist '()) (task-alist '()))

      ;; Find entity end
      (let ((depth 1))
        (while (progn
                 (re-search-forward (concat "^\\s-*\\(end" entity-type "\\|" entity-type "\\)") nil t)
                 (if (string= (verilog-match-string 1) entity-type)
                     (setq depth (1+ depth))
                   (setq depth (1- depth)))
                 (> depth 0))))
      (setq entity-end (point-at-eol))
      ;;(setq entity-end (point-max))
      ;; Work through entity
      (goto-char entity-start)
      (end-of-line)
      (while (< end entity-end)

        ;; Look for a nested entity or routine
        (save-excursion
          (if (re-search-forward "^\\s-*\\(module\\|interface\\|package\\|class\\|function\\|task\\)[ \t\n]+\\([a-zA-Z0-9_:]+\\)" entity-end t)
              (let ((found-item (verilog-match-string 1)))
                (setq end (point-at-bol))
                (if (or (string= found-item "function") (string= found-item "task"))
                    (progn
                      (beginning-of-line)
                      (re-search-forward "^\\s-*\\(function\\|task\\)\\s-+\\(?:[a-z]+\\s-+\\)?\\([a-zA-Z0-9_:]+\\)\\s-*[(;]" nil t)
                      (if (string= found-item "function")
                          (push (cons (verilog-match-string 2) (point-at-bol)) function-alist)
                        (push (cons (verilog-match-string 2) (point-at-bol)) task-alist))
                      (setq nested-entity nil
                            found-routine t
                            routine-type found-item))
                  (setq nested-entity t
                        found-routine nil)))
            (setq nested-entity nil
                  found-routine nil
                  end entity-end)))

        ;; Find instances or modports
        (if (string= entity-type "interface")
            (setq modport-alist (append modport-alist (verilog-imenu-create-find-instances-or-modports end)))
          (setq instance-alist (append instance-alist (verilog-imenu-create-find-instances-or-modports end))))

        ;; Find enums, structs, and unions
        (setq enum-alist (append enum-alist (verilog-imenu-create-find-data-types "enum" end)))
        (setq struct-alist (append struct-alist (verilog-imenu-create-find-data-types "struct" end)))
        (setq union-alist (append union-alist (verilog-imenu-create-find-data-types "union" end)))

        ;; Goto next point of interest
        (goto-char end)

        ;; If a routine was found, jump over it
        (when found-routine
          (re-search-forward (concat "^\\s-*end" routine-type) entity-end t)
          (end-of-line))

        ;; If a nested entity was found, parse it
        (when nested-entity
          (let (sub-entity)
            (setq sub-entity (verilog-imenu-create-parse-entity))
            (when sub-entity
              (let ((entity-type (car sub-entity)) (entity-info (cdr sub-entity)))
                (cond ((string= entity-type "module")
                       (push entity-info module-alist))
                      ((string= entity-type "interface")
                       (push entity-info interface-alist))
                      ((string= entity-type "package")
                       (push entity-info package-alist))))))))

      ;; Assemble
      (if verilog-imenu-flatten
          (progn
            (push (cons entity-name entity-start) final-alist)
            (setq final-alist (verilog-imenu-add-flattened entity-name instance-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name modport-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name task-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name function-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name union-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name struct-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name enum-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name package-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name interface-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name module-alist final-alist))
            (goto-char entity-end)
            final-alist)
        (push (cons "*Definition*" entity-start) final-alist)
        (setq final-alist (verilog-imenu-create-add-item-alist "Instances" instance-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Modports" modport-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Tasks" task-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Functions" function-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Unions" union-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Structs" struct-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Enums" enum-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Packages" package-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Interfaces" interface-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "Modules" module-alist final-alist))
        (goto-char entity-end)
        (cons entity-type (cons entity-name final-alist))))))

(defun verilog-imenu-add-flattened (name alist final-alist)
  (mapc (lambda (item)
          (if verilog-imenu-qualify-names
              (push (cons (concat name "::" (car item)) (cdr item)) final-alist)
            (push (cons (car item) (cdr item)) final-alist)))
        alist)
  final-alist)

(defun verilog-imenu-create-index-function ()
  "Create Verilog imenu index."
  (goto-char (point-min))
  (let ((final-alist '()) (module-alist '()) (interface-alist '()) (package-alist '())
        (entity))
    (while (progn
             (setq entity (verilog-imenu-create-parse-entity))
             (when entity
               (if verilog-imenu-flatten
                   (mapc (lambda (x) (push x final-alist)) entity)
                 (let ((entity-type (car entity)) (entity-info (cdr entity)))
                   (cond ((string= entity-type "module")
                          (push entity-info module-alist))
                         ((string= entity-type "interface")
                          (push entity-info interface-alist))
                         ((string= entity-type "package")
                          (push entity-info package-alist))))))))
    (if verilog-imenu-flatten
        (setq final-alist (verilog-sort-alist-by-car-string final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Packages" package-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Interfaces" interface-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Modules" module-alist final-alist)))
    final-alist))

;;; Align

(require 'align)

(defcustom align-verilog-rules-list
  `(
    (verilog-declaration
     (regexp . "\\(logic\\|input\\|output\\|inout\\|wire\\|reg\\)\\(\\s-+[[][^]]+[]]\\|\\)\\(\\s-+\\)\\S-")
     (group . (3)))

    (verilog-asgn_param
     (regexp . "\\(assign\\|parameter\\)\\(\\s-+\\)\\S-")
     (group . (2)))

    (verilog-assign
     (regexp . "\\S-+\\(\\s-*\\)[!=><]+\\(\\s-*\\)\\S-")
     (group . (1 2)))

    (verilog-ports-no-comment
     (regexp . "[.][a-zA-Z0-9_]+\\(\\s-+\\)\\S-")
     (group . (1)))

    (verilog-ports-comment
     (regexp . "[.][a-zA-Z0-9_]+\\(\\s-+\\)\\S-.*\\(\\s-+\\)[/]+")
     (group . (1 2)))
    )
  "Verilog alignment rules."
  :type align-rules-list-type
  :group 'align)

(defcustom align-exclude-verilog-rules-list
  `(
    (exc-dq-string
     (regexp . "\"\\([^\"\n]+\\)\"")
     (repeat . t)
     (modes . align-dq-string-modes))

    (exc-open-comment
     (regexp . ,(function (lambda (end reverse)
        (funcall (if reverse 're-search-backward 're-search-forward)
                 (concat "[^ \t\n\\\\]" (regexp-quote comment-start)
                         "\\(.+\\)$") end t))))
     (modes . align-open-comment-modes))
    )
  "Verilog alignment exclusion rules."
  :type align-exclude-rules-list-type
  :group 'align)

;;(put 'align-verilog-rules-list 'risky-local-variable t)
;;(put 'align-exclude-verilog-rules-list 'risky-local-variable t)

;;(add-to-list 'align-dq-string-modes 'verilog-mode)
;;(add-to-list 'align-open-comment-modes 'verilog-mode)

;;; Hook

(defvar verilog-port-menu
      '("VerilogPorts"
        ["Copy port" my-verilog-port-copy t]
        ["Paste port as instance" my-verilog-port-paste-inst (and my-verilog-inst-name my-verilog-inst-ports)]
        ["Paste port wires" my-verilog-port-paste-wires my-verilog-inst-ports]
        )
      "Verilog port helper functions")

(defun verilog-extras-hook ()
  ;;(define-key verilog-mode-map "\M-*" nil)
  ;;(define-key verilog-mode-map ":" nil)
  ;;(modify-syntax-entry ?` ".")
  ;; (local-set-key (kbd "C-=") 'verilog-sk-nonblock-assign)
  (if (string-match "XEmacs" emacs-version)
      (add-submenu nil verilog-port-menu)
    (easy-menu-add-item verilog-mode-map '("menu-bar") verilog-port-menu))
  (setq imenu-generic-expression nil)
  (setq imenu-create-index-function 'verilog-imenu-create-index-function)
  ;;(setq align-mode-rules-list align-verilog-rules-list)
  ;;(setq align-exclude-rules-list align-exclude-verilog-rules-list)
  )



;;----------------------------------------------------------------------------
;;   reconfig verilog-mode
;;----------------------------------------------------------------------------

(setq verilog-date-scientific-format 1)
;;; compilation-error-regexp-alist
(setq my-verilog-compilation-error-regexp-alist
   '(
  ;;   ;;(name "regexp" file line column type[nil/2:error 1:warning 0:info   ])
     (verilog-IES-error
      ".*\\*[E],[0-9A-Z]+\\(\[[0-9A-Z_,]+\]\\)? (\\([^ \t,]+\\),\\([0-9]+\\)|\\([0-9]+\\)" 2 3 4 2)
  ;;   (verilog-IES-warn
  ;;   ".*\\*[WE],[0-9A-Z]+\\(\[[0-9A-Z_,]+\]\\)? (\\([^ \t,]+\\),\\([0-9]+\\)|\\([0-9]+\\)" 2 3 4 1)
  ;;   (verilog-ncvlog-warn
  ;;    ".*\\*[W],[0-9A-Z]+ (\\([^ \t,]+\\),\\([0-9]+\\)|\\([0-9]+\\)" 1 2 3 1)
     (verilog-IES-assert
      ".*\\*[EW],[0-9A-Z]+ (\\([^ ,]+\\),\\([0-9]+\\)|?\\([0-9]+\\)?):" 1 2 3 2)
  ;;   (verilog-violation
  ;;    "File: \\(.*\\), line = \\([0-9]+\\)" 1 2 nil 1)
     (verilog-line-of-file
      "line \\([0-9]+\\) of \\([^ \t\n,]+.[a-zA-Z0-9]\\)" 2 1 nil 0)
  ;;   (verilog-ncsim
  ;;    "\\*[EW],[A-Za-z]+ (\\([^ \t\n,]+\\),\\([0-9]+\\))" 1 2)
     (uvm_info
      "UVM_INFO \\([^ \t\n]+\\)(\\([0-9]+\\))" 1 2 nil 0)
     (uvm_fatal
      "UVM_FATAL \\([^ \t\n]+\\)(\\([0-9]+\\))" 1 2 nil 2)
     (uvm_warn
      "UVM_WARNING \\([^ \t\n]+\\)(\\([0-9]+\\))" 1 2 nil 1)
     (uvm_error
      "UVM_ERROR \\([^ \t\n]+\\)(\\([0-9]+\\))" 1 2 nil 2)
     (verilog-nc-file
      "[Ff]ile: \\([^ \t\n,]+\\)" 1 nil nil 0)
     (verilog-nc-file-line
      "[Ff]ile: \\([^ \t\n,]+\\) line \\([0-9]+\\)" 1 2 nil 0)
     (verilog-ncelab-infor
      "ncelab: (\\([^ \t\n,]+\\),\\([0-9]+\\)):" 1 2 nil 0)
     (ncelab-sdf-warn
      "ncelab.*<\\([^ \t\n,]+\\), line \\([0-9]+\\)>" 1 2 nil 1)
     ;; (verilog-ncsim-file
     ;;  "\\([^ \t\n,]+\\):\\([0-9]+\\)" 1 2 nil 0)
    ) )
;;  "List of regexps for Verilog compilers.
;;See `compilation-error-regexp-alist' for the formatting.  For Emacs 22+.")

;; Following code only gets called from compilation-mode-hook on Emacs to add error handling.

(remove-hook 'compilation-mode-hook 'verilog-error-regexp-add-emacs)
(if (featurep 'emacs)
    (add-hook 'compilation-mode-hook
              (lambda ()
                (my-config-error-regexp-add-emacs my-verilog-compilation-error-regexp-alist))))
(define-skeleton verilog-sk-uvm-component
  "Insert a class definition"
  ()
  > "class " (setq name (skeleton-read "Name: ")) " extends " (skeleton-read "Extends: ") ";" \n
  > _ \n
  > "`uvm_component_utils_begin(" name ")" \n
  > (- verilog-indent-level) "`uvm_component_utils_end" \n
  > _ \n
  > "function new(string name=\"" name "\", uvm_component parent);" \n
  > "super.new(name, parent);" \n
  > (- verilog-indent-level) "endfunction" \n
  > _ \n
  > "endclass" (progn (electric-verilog-terminate-line) nil))

(define-skeleton verilog-sk-begin
  "Insert begin end block.  Uses the minibuffer to prompt for name."
  ()
  ;; '(delete-horizontal-space)
  '(verilog-indent-line-relative)
  '(end-of-line)
  "begin"\n
  > _ \n
  > (- verilog-indent-level-behavioral) "end"
  )

(define-skeleton verilog-sk-module
  "Insert a module definition."
  ()
  > "module " (file-name-base) " (/*AUTOARG*/ ) ;" \n
  > _ \n
  > (- verilog-indent-level-behavioral) "endmodule" (progn (electric-verilog-terminate-line) nil))

(defun fill-column-with-x(max-column x)
  (interactive)
  (verilog-indent-line-relative)
  (end-of-line)
  (while (< (current-column) max-column)
    (insert x)))

(defun verilog-sk-comment()
  "Inserts three comment lines, making a display comment."
  (interactive)
  (insert "/*")(verilog-indent-line-relative)
  (fill-column-with-x 80 "*")(newline-and-indent)
  (insert "*")  (newline-and-indent)
  (insert "*") (setq char-pos(point)) (newline-and-indent)
  (insert "*")  (newline-and-indent)
  (fill-column-with-x 79 "*") (insert "/")
  (goto-char (- char-pos 1))
  (end-of-line)
  )

;; (define-skeleton verilog-sk-comment
;;   "Inserts three comment lines, making a display comment."
;;   ()
;;   > "/****************************************************************************\n"
;;   > "*\n"
;;   > "* " _ \n
;;   > "*\n"
;;   > "***************************************************************************/")

(define-skeleton verilog-sk-nonblock-assign
  "insert \"<= #1\""
  > " <= #1 " _)
;;(local-set-key "C-," 'verilog-sk-nonblock-assign)

(define-skeleton file-name
  "Insert a module definition."
  ()
   (file-name-base)_ )


(defadvice verilog-header (around my-verilog-header
                                  (&optional opt_arg))
  "Insert a standard Verilog file header.
See also `verilog-sk-header' for an alternative format."
  (goto-line 1)
  (interactive)
  (let ((start (point)))
    (insert "////////////////////////////////////////////////////////////////////////////////
//   __  ______
//  |  ||____  \\     Company     : CHENGDU IMAGE DESIGN
//  |  |     \\  \\    Last Update : <lastdate>
//  |  |     |  |    Module Name : <module>
//  |  | ____/  /    Project Name: <project>
//  |__||______/     Engineer    : <author>
//
// Additional Comments: <comments>
//
// Copyright (c) <year> by <company>. This model is the confidential and
// proprietary property of <company>. and the possession or use of this
// file requires a written license from <company>.
////////////////////////////////////////////////////////////////////////////////
//
// Version:
//  <lastdate>: Created.
//

")
    (goto-char start)
    (let (string)
      (setq string (read-string "Company: "   "ImageDesign"))
      (setq verilog-company string)
      ;;(search-forward "<company>") (replace-match string t t)
      (search-forward "<company>") (replace-match string t t)
      (search-forward "<company>") (replace-match string t t)
      (search-forward "<company>") (replace-match string t t)
      (goto-char start)
      (search-forward "<year>") (replace-match "" t t)
      (verilog-insert-year)
      (goto-char start)
      (search-forward "<lastdate>") (replace-match "" t t)
      (verilog-insert-time)
      (goto-char start)
      (setq string (read-string "module: " (file-name-base)))
      (search-forward "<module>") (replace-match string t t)
      (setq string (read-string "project: " (projectile-project-name)))
      (setq verilog-project string)
      (search-forward "<project>") (replace-match string t t)
      (search-forward "<author>") (replace-match "" t t)
      (insert (user-full-name))
      (insert "<" (user-login-name) "@" (system-name) ">")
      (search-forward "<lastdate>") (replace-match "" t t)
      (insert (format-time-string "%Y-%m-%d"))
      (search-backward "<comments>") (replace-match "" t t)

    )))
(ad-activate 'verilog-header)

(defadvice verilog-star-comment(around my-verilog-star-comment)
  (interactive)
  (verilog-indent-line)
  (insert "/*")
  (save-excursion
    (newline)
    (verilog-indent-line)
    (insert "*/"))
  (newline)
  (verilog-indent-line)
  (insert "* "))
(ad-activate 'verilog-star-comment)

(defun  my-verilog-indent-line-relative()
  (save-excursion
    (verilog-indent-line-relative)
   )
)
(defun verilog-insert-time ()
  "Insert date from the system."
  (interactive)
  (if verilog-date-scientific-format
      ;;(insert (format-time-string "%Y/%m/%d-%H:%M:%S"))
      (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
    (insert (format-time-string "%d.%m.%Y-%H:%M:%S"))))

(defadvice verilog-inject-auto (before verilog-last-update
                                       (&optional opt_arg))
  "update \"Last Update:\" before 'verilog-inject-auto"
  (save-excursion
    (goto-line 1)
    (search-forward "Last Update : ") (kill-line)
    (verilog-insert-time)))
(ad-activate 'verilog-inject-auto)


(defun my-verilog-insert-seq-always ()
  "Create a always block of sequence..."
  (interactive)
  (setq pre-clk "" post-clk "" cur-clk ""
        pre-rst "" post-rst "" cur-rst "")
  (progn
    (save-excursion
      (when (verilog-re-search-backward "\\bposedge\\b" nil t)
        (skip-syntax-forward "w_")
        (when (verilog-re-search-forward "[a-zA-Z][a-zA-Z0-9_]*" nil t)
          (setq pre-clk (match-string-no-properties 0))))
      (when (verilog-re-search-forward "\\bposedge\\b" nil t)
        (skip-syntax-forward "w_")
        (when (verilog-re-search-forward "[a-zA-Z][a-zA-Z0-9_]*" nil t)
          (setq post-clk (match-string-no-properties 0))))
      (if (string< "" pre-clk)
          (setq cur-clk pre-clk)
        (setq cur-clk post-clk))
      (if (string= cur-clk "")
          (setq cur-clk "SCLK"))

      (when (verilog-re-search-backward "\\bnegedge\\b" nil t)
        (skip-syntax-forward "w_")
        (when (verilog-re-search-forward "[a-zA-Z][a-zA-Z0-9_]*" nil t)
          (setq pre-rst (match-string-no-properties 0))))
      (when (verilog-re-search-forward "\\bnegedge\\b" nil t)
        (skip-syntax-forward "w_")
        (when (verilog-re-search-forward "[a-zA-Z][a-zA-Z0-9_]*" nil t)
          (setq post-rst (match-string-no-properties 0))))
      (if (string< "" pre-rst)
          (progn
            (setq cur-rst pre-rst))
        (setq cur-rst post-rst))
      (if (string= cur-rst "")
          (setq cur-rst "RSTN"))
      )
    (verilog-indent-line-relative)
    (insert
     "always @(posedge cur-clk or negedge cur-rst) begin
      if(!cur-rst)begin
         /*AUTORESET*/
      end
      else begin
         <last-edit>
      end
   end"))
  (search-backward "cur-clk") (replace-match cur-clk t t)
  (search-forward "cur-rst") (replace-match cur-rst t t)
  (search-forward "cur-rst") (replace-match cur-rst t t)
  (search-forward "<last-edit>") (replace-match "" t t)
;;  (verilog-indent-buffer)
  )
(defun my-verilog-insert-tb (mod_name)
  (interactive)
  (insert (format  "module %s %s" (format "%s_tb" mod_name) " (/*AUTOARG*/);"))
  (insert "

   /*AUTOREGINPUT*/
   /*AUTOWIRE*/

   <REFERENCE> uut(/*AUTOINST*/);

   initial begin : STIMULUS
      /*$sdf_annotate ( <\"sdf_file\">, <module_instance>?,
      <\"config_file\">?, <\"log_file\">?, <\"mtm_spec\">?,
      <\"scale_factors\">?, <\"scale_type\">? );*/
      $shm_open(\"wave\");\n")
  (insert    "      $shm_probe(" my-verilog-inst-name  "_tb" ",\"ASC\");\n");
  (insert "
      <stimulus>
   end

endmodule // tb

// Local Variables:
// verilog-library-flags:(\"-y ../\")
// End:")

  )

(defun my-verilog-create-tb()
  "Create a testbench file for current module..."
  (interactive)
  (setq my-verilog-inst-name (my-verilog-get-module-name))
  (setq my-verilog-inst-ports (verilog-read-decls))
  (switch-to-buffer (format "%s_tb.v" my-verilog-inst-name))
  (verilog-mode)
  (my-verilog-insert-tb my-verilog-inst-name)
  (goto-line 1)
  (search-forward "<REFERENCE>") (replace-match my-verilog-inst-name t t);

  ;; insert values table
  ;; (search-forward "<table>") (replace-match "" t t);
  ;; (insert "#+NAME: TABLE0\n")
  ;; (insert "|Ports\\\No.|0|\n")
  ;; (insert "|-")
  ;; (dolist (port (verilog-decls-get-inputs my-verilog-inst-ports))
  ;;   (insert (concat "|" (car port) "| \n")))

  ;; insert default values of input port
  (search-forward "<stimulus>") (replace-match "" t t);
  (verilog-indent-line-relative)
  (dolist (port (verilog-decls-get-inputs my-verilog-inst-ports))
    (insert (concat (car port) " = 'd0;\n"))
    (verilog-indent-line-relative)))



;;; keybings
;;(define-key verilog-template-map (kbd ",") 'verilog-sk-nonblock-assign)
;; (setq verilog-mode-abbrev-table nil)
;; (define-abbrev-table 'verilog-mode-abbrev-table ())
(define-skeleton verilog-sk-always
  "Insert seq block"
  ()
  > "always @ ( " (read-string "clock: " "posedge CLK" ) " or " (read-string "reset: " "negedge RSTN")  " ) begin\n"
  > "if(!RSTN) begin\n"
  > "/*AUTORESET*/\n"
  > (- verilog-indent-level-behavioral) "end\n"
  > '(electric-verilog-tab) "else begin\n"
  > _ \n
  > (- verilog-indent-level-behavioral) "end"
  > (- verilog-indent-level-behavioral) "end" \n>
  )
(verilog-define-abbrev verilog-mode-abbrev-table "class_uvm_compoent"  "" 'verilog-sk-uvm-component)
(verilog-define-abbrev verilog-mode-abbrev-table "class_uvm_object"    "" 'verilog-sk-uvm-object)
(verilog-define-abbrev verilog-mode-abbrev-table "begin"               "" 'verilog-sk-begin)

;; (defalias 'my-verilog 'ignore)
;; (provide 'my-verilog)

;;;###autoload
(define-minor-mode my-verilog
  "It is my configuration of verilog-mode"
  :group prog-mode
  :init-value nil
  ;; :lighter "my-verilog"
  :global nil
  :after-hook verilog-mode-hook
  (verilog-extras-hook)
  (setq verilog-auto-lineup 'all)
  (setq verilog-assignment-delay "#1 ")
  ;; (set (make-local-variable 'indent-line-function)
  ;;      #'my-verilog-indent-line-relative)
  (local-set-key (kbd "C-=") 'verilog-sk-nonblock-assign)
 )
