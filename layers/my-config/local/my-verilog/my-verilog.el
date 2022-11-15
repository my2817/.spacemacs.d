;;; my-verilog-extras.el


;;; Enhancements to verilog-mode

(require 'verilog-mode)
(unless (fboundp 'hs-special-modes-alist)
  (defvar hs-special-modes-alist nil))

;; remove origin verilog-mode from hs-special-modes-alist
(let* (tmp)
  (dolist (mode-special hs-special-modes-alist)
    (if (not (string= (car mode-special) "verilog-mode"))
        (add-to-list 'tmp mode-special)))
  (setq hs-special-modes-alist tmp))
(add-to-list 'hs-special-modes-alist '(verilog-mode  "\\<begin\\>\\|\\<task\\>\\|\\<function\\>\\|\\<class\\>\\|\\<interface\\>\\|\\<fork\\>\\|(\\|\\<`ifdef\\>\\|\\<`ifndef\\>\\|{"
                                                     "\\<end\\>\\|\\<endtask\\>\\|\\<endfunction\\>\\|\\<endclass\\>\\|\\<endinterface\\>\\|\\<join\\>\\|)\\|\\<`endif\\>\\|}"
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
      (while (verilog-re-search-forward
              "^\\s-*\\([a-zA-Z0-9_]+\\)\\([ \t\n]+#([-+/a-zA-Z0-9._*,`() \t\n']*)\\)?[ \t\n]+\\([a-zA-Z0-9_]+\\)\\([ \t\n]*\\[[]0-9a-zA-Z: `\t\n_-']*\\)?[ \t\n]*("
              end t)
        (condition-case nil
            (let ((instance-type (verilog-match-string 1)) (instance-name (verilog-match-string 3))
                  (instance-pos (match-beginning 0)))
              (if (not (string= instance-type "else"))
                  (progn
                    (backward-char)
                    ;; goto end of instance entity
                    (goto-char (my-verilog-search-pair-end-position "(" ")"))
                    (verilog-re-search-forward "[ \t\n]*;" (point-at-eol) t)
                    (backward-char)
                    (when (looking-at "[ \t\n]*;")
                      (if (string= instance-type "modport")
                          (push (cons instance-name instance-pos) instance-alist)
                        (if verilog-imenu-show-instance-type
                            (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
                          (push (cons instance-name instance-pos) instance-alist)))))))
          (error nil)))
      instance-alist)))

(defun verilog-imenu-create-find-begin-process (end)
  (save-excursion
    (let ((instance-alist '()))
      (while (re-search-forward
              "\\s-*\\(begin\\)\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)\\s-*"
              end t)
        (condition-case nil
            (let ((instance-type (verilog-match-string 1)) (instance-name (verilog-match-string 2))
                  (instance-pos (match-beginning 0)))
              (backward-char)
              (forward-sexp)
              (if verilog-imenu-show-instance-type
                  (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
                (push (cons instance-name instance-pos) instance-alist)))
          (error nil)))
      instance-alist)))

(defun verilog-imenu-create-find-covergroup (end)
  (save-excursion
    (let ((instance-alist '()))
      (while (verilog-re-search-forward
              "\\s-*\\(covergroup\\)\\s-+\\([a-zA-Z0-9_]+\\)\\s-*"
              end t)
        (condition-case nil
            (let ((instance-type (verilog-match-string 1)) (instance-name (verilog-match-string 2))
                  (instance-pos (match-beginning 0)))
              (backward-char)
              (forward-sexp)
              (if verilog-imenu-show-instance-type
                  (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
                (push (cons instance-name instance-pos) instance-alist)))
          (error nil)))
      instance-alist)))

(defun verilog-imenu-create-find-coverpoint (end)
  (save-excursion
    (let ((instance-alist '()))
      (while (verilog-re-search-forward
              "\\s-*\\([a-zA-Z0-9_]+\\)\\s-*:\\s-*\\(coverpoint\\|cross\\)\\s-*"
              end t)
        (condition-case nil
            (let ((instance-type (verilog-match-string 2)) (instance-name (verilog-match-string 1))
                  (instance-pos (match-beginning 0)))
              (backward-char)
              (forward-sexp)
              (if verilog-imenu-show-instance-type
                  (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
                (push (cons instance-name instance-pos) instance-alist)))
          (error nil)))
      instance-alist)))

(defun verilog-imenu-create-find-ifdef (end)
  (save-excursion
    (let ((instance-alist '()))
      (while (verilog-re-search-forward
              "\\(`ifdef\\|`ifndef\\|`elsif\\)\\s-*\\([a-zA-Z0-9_]+\\)\\s-*"
              end t)
        (condition-case nil
            (let ((instance-type (verilog-match-string 1)) (instance-name (verilog-match-string 2))
                  (instance-pos (match-beginning 0)))
              (backward-char)
              (forward-sexp)
              (if verilog-imenu-show-instance-type
                  (push (cons (concat instance-name " <" instance-type ">") instance-pos) instance-alist)
                (push (cons instance-name instance-pos) instance-alist)))
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
  (when (re-search-forward "^\\s-*\\(module\\|interface\\|package\\|class\\|function\\|task\\|`ifdef\\|`ifndef\\||`elsif\\)[ \t\n]+\\([a-zA-Z0-9_:]+\\)" nil t)
    (let ((entity-type  (verilog-match-string 1))
          (entity-name
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
          (enum-alist '()) (struct-alist '()) (union-alist '()) (function-alist '()) (task-alist '())
          (begin-alist '()) (covergroup-alist '()) (coverpoint-alist '()) (compile-directive-alist '()))

      ;; Find entity end
      (let ((depth 1))
        (while (progn
                 (if (or (string= entity-type "`ifdef")
                         (string= entity-type "`ifndef"))
                     (re-search-forward (concat "^\\s-*\\("  "`endif" "\\)") nil t)
                   (re-search-forward (concat "^\\s-*\\(end" entity-type "\\|" entity-type "\\)") nil t))
                 (if (string= (verilog-match-string 1) entity-type)
                     (setq depth (1+ depth))
                   (setq depth (1- depth)))
                 (> depth 0))))
      (setq entity-end (point-at-eol))
      ;;(setq entity-end (point-max))
      ;; Work through entity
      (goto-char entity-start)
      (end-of-line)
      ;; befor jump over routine,parse begin-process
      (setq begin-alist (append begin-alist (verilog-imenu-create-find-begin-process entity-end)))
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
        ;; (setq begin-alist (append begin-alist (verilog-imenu-create-find-begin-process end)))
        (setq covergroup-alist (append covergroup-alist (verilog-imenu-create-find-covergroup end)))
        (setq coverpoint-alist (append coverpoint-alist (verilog-imenu-create-find-coverpoint end)))
        (setq compile-directive-alist (append compile-directive-alist (verilog-imenu-create-find-ifdef end)))

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
              (let ((entity-type (caar sub-entity)) (entity-info (cdr sub-entity)))
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
            (setq final-alist (verilog-imenu-add-flattened entity-name begin-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name covergroup-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name coverpoint-alist final-alist))
            (setq final-alist (verilog-imenu-add-flattened entity-name compile-directive-alist final-alist))
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
        (setq final-alist (verilog-imenu-create-add-item-alist "Process" begin-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "CoverGroup" covergroup-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "CoverPoint" coverpoint-alist final-alist))
        (setq final-alist (verilog-imenu-create-add-item-alist "CompileDirec" compile-directive-alist final-alist))
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
        (anonymous-alist '()) (class-alist '()) (function-alist '()) (task-alist '())
        (direc-alist '())
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
                          (push entity-info package-alist))
                         ((string= entity-type "class")
                          (push entity-info class-alist))
                         ((string= entity-type "function")
                          (push entity-info function-alist))
                         ((string= entity-type "task")
                          (push entity-info task-alist))
                         ((string= entity-type "`ifdef")
                          (push entity-info direc-alist))
                         (t
                          (push entity-info anonymous-alist))))))))
    (if verilog-imenu-flatten
        (setq final-alist (verilog-sort-alist-by-car-string final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Packages" package-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Interfaces" interface-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Modules" module-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Classes" class-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Functions" function-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Tasks" task-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "CompileDirec" direc-alist final-alist))
      (setq final-alist (verilog-imenu-create-add-item-alist "Anon" anonymous-alist final-alist))
      )
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
  (hs-minor-mode 1)
  )

(defvar my-verilog-menu
  '("My-config"
    ["Create Testbench" my-verilog-create-tb t]
    ["Align Inst" my-verilog-align-indent-inst-signal t]
    ["cnfonts size" cnfonts-edit-profile t]
    ["Init .dir-local" my-project-dir-local-init t]
    )
  "misc function list")
(easy-menu-add-item verilog-mode-map '("menu-bar") my-verilog-menu)

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
  > "module " (file-name-base (or (buffer-file-name) (buffer-name))) " (/*AUTOARG*/ ) ;" \n
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
                                  (&optional quiet))
  "Insert a standard Verilog file header.
See also `verilog-sk-header' for an alternative format.
QUIET: be quiet and don't promote anything if t"
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
    (let (compnay-name module-name project-name)
      (if (not quiet)
          (progn
            (setq company-name (read-string "Company: "   "ImageDesign"))
            (setq module-name (read-string "module: " (file-name-base (or (buffer-file-name) (buffer-name)))))
            (setq project-name (read-string "project: " (projectile-project-name)))
            )
        (progn
          (setq company-name "ImageDesign")
          (setq module-name  (file-name-base (or (buffer-file-name) (buffer-name))))
          (setq project-name (projectile-project-name))
          ))
      (setq verilog-company company-name)
      ;;(search-forward "<company>") (replace-match string t t)
      (search-forward "<company>") (replace-match company-name t t)
      (search-forward "<company>") (replace-match company-name t t)
      (search-forward "<company>") (replace-match company-name t t)
      (goto-char start)
      (search-forward "<year>") (replace-match "" t t)
      (verilog-insert-year)
      (goto-char start)
      (search-forward "<lastdate>") (replace-match "" t t)
      (verilog-insert-time)
      (goto-char start)
      (search-forward "<module>") (replace-match module-name t t)
      (setq verilog-project project-name)
      (search-forward "<project>") (replace-match project-name t t)
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

(defun my-verilog-pretty-autoreset()
  "Search region of AUTORESET, then excute `verilog-pretty-expr'"
  (interactive)
  (when (or (eq 'all verilog-auto-lineup)
            (eq 'assignment verilog-auto-lineup))
    (save-excursion
      (goto-line 1)
      (while (re-search-forward "/\\*autoreset\\*/\\|/\\*AUTORESET\\*/" nil t)
        (next-line)
        (beginning-of-line)
        (when (search-forward "// Beginning of autoreset for uninitialized flops" (point-at-eol) t)
          (next-line)
          (verilog-pretty-expr))))))

;; (defadvice verilog-auto (after verilog-last-update
;;                                (&optional opt_arg))
;;   "update \"Last Update:\" before 'verilog-inject-auto.
;; see also:
;; `my-verilog-auto-align'
;; `my-verilog-align-indent-inst-signal'
;; "
;;   (save-excursion
;;     (goto-line 1)
;;     (if (buffer-modified-p)
;;         (if (search-forward "Last Update : " nil t)
;;             (progn
;; (delete-region (point) (point-at-eol))
;;               (verilog-insert-time))
;;           (message "Can't find the position to update the \"last updated timing\""))))
;;   (imenu-list-rescan-imenu)
;;   (if my-verilog-auto-align
;;       (my-verilog-align-indent-inst-signal))
;;   (my-verilog-pretty-autoreset)
;;   )
;; (ad-activate 'verilog-auto)

(defcustom verilog-auto-before-hook nil
  "Hook run after graphviz-dot-mode is loaded"
  :type 'hook
  :group 'verilog-mode)

(defun my-verilog-auto ()
  "Do something before run `verilog-auto'
use `verilog-auto-hook' to do something after that
"
  (interactive)
  (let ((flycheck-status (if (fboundp 'flycheck-mode) flycheck-mode nil)))
    (if flycheck-status
        (flycheck-mode 0))
    (run-hooks verilog-auto-before-hook)
    (verilog-auto)
    (save-excursion
      (goto-line 1)
      (if (buffer-modified-p)
          (if (search-forward "Last Update : " nil t)
              (progn
                (delete-region (point) (point-at-eol))
                (verilog-insert-time))
            (message "Can't find the position to update the \"last updated timing\""))))
    (imenu-list-rescan-imenu)
    (if my-verilog-auto-align
        (my-verilog-align-indent-inst-signal))
    (my-verilog-pretty-autoreset)
    (if flycheck-status
        (flycheck-mode 1)
      (flycheck-mode 0)
      )
    )
  (imenu-list-update)
  )

(define-key verilog-mode-map "\C-c\C-a" 'my-verilog-auto)


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
  (goto-char (point-max))
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
(verilog-define-abbrev verilog-mode-abbrev-table "class_uvm_component"  "" 'verilog-sk-uvm-component)
(verilog-define-abbrev verilog-mode-abbrev-table "class_uvm_object"    "" 'verilog-sk-uvm-object)
(verilog-define-abbrev verilog-mode-abbrev-table "begin"               "" 'verilog-sk-begin)

;; (defalias 'my-verilog 'ignore)
;; (provide 'my-verilog)
(defun my-verilog-get-last-history-log ()
  "Return the history log of current module"
  (interactive)
  (save-excursion
    (goto-char 0)
    (if (search-forward "// Version:" nil t)
        (progn
          (next-line) (beginning-of-line)
          (search-forward-regexp "[^/0-9-@: ]")
          (backward-char)
          (let* ((lin-context (buffer-substring-no-properties (point) (line-end-position))))
            (message lin-context)))
      (message "NO ci log"))
    )
  )


(defcustom my-verilog-min-spc-for-align 1
  "submodule uut (
               .PORT1 <-min-spc-> (signal1),
               .PORT2 <-min-spc-> (signal2))"
  :group 'verilog-mode-indent)
(defcustom my-verilog-auto-align t
  "true to excute `my-verilog-align-indent-inst-signal' after `verilog-auto'
add the following lines into the verilog file's end to disable it:
// Local Variables:
// my-verilog-auto-align:nil
// End:
"
  :group 'verilog-mode-indent)
(defun my-verilog-align-indent-inst-signal ()
  "align all inst entity's left-pair(parameter entity included) and all inst's port list
Please make sure all inst's port is format with style of one port one line. Eg.:
-----------------------------------
subA u_subA(
           .a(a_wire),
           .bb(bb_wire),
           .ccc(ccc_wire)
           );
-----------------------------------
and the result of this function should be:
-----------------------------------
subA u_subA(
           .a   (a_wire),
           .bb  (bb_wire),
           .ccc (ccc_wire)
           );
-----------------------------------

use `my-verilog-min-spc-for-align' to control the minimum space between port and signal name.
see also:
`my-verilog-auto-align'

imp step:
1. get entity of all inst, and the max length of all ports(parameter symbols included)
2. align all inst entity's left-pair, and then indent all port inside pair
3. align all signals in port list
"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((inst-alist '()) (final-alist '())
          (pos-inst-start '0) (port-length '0) (pos-port-right-paren '0) (length-current '0) (pos-parent-end '0)
          (left-pair "(") (right-pair ")")
          (inst-count 0) (inst-index 0))
      ;; remove module entity from inst-alist
      (setq inst-alist (verilog-imenu-create-find-instances-or-modports (point-max)))
      (message "calculation column position for: %d" (setq inst-count (length inst-alist)))
      (dolist (entity inst-alist) ;; get maxmum column of insts and ports
        (let ((param-entiy)
              (inst-name (car (split-string (car entity))))
              (inst-type (cdr (split-string (car entity))))
              (inst-pos (cdr entity)))
          ;; jump through module entity
          (if (not (string= (car inst-type) "<module>"))
              (progn
                (push entity final-alist)
                ;; search most right position of left-pair
                (goto-char inst-pos)
                (beginning-of-line)
                ;; parameter list
                (if (verilog-re-search-forward "#(" (point-at-eol) t)
                    (progn
                      (setq param-entiy t)
                      (verilog-re-search-backward "[a-zA-Z0-9_]" (point-at-bol) t)
                      (if (< pos-inst-start (+ 3 (current-column)))
                          (setq pos-inst-start (+ (current-column) 3)));; "#(" has two chars
                      ;; get max-column of parameter list
                      (verilog-re-search-forward "#(" (point-at-eol) t) (backward-char)
                      (setq pos-parent-end (my-verilog-search-pair-end-position left-pair right-pair))
                      (setq length-current (my-verilog-port-max-length-by-search-parent left-pair right-pair))
                      (if (< port-length length-current)
                          (setq port-length length-current))
                      (goto-char pos-parent-end)
                      (next-line)
                      (beginning-of-line))
                  (setq param-entiy nil))
                ;; get max-column of signal port list
                (verilog-re-search-forward "(" nil t)
                (verilog-re-search-backward "[a-zA-Z0-9_]" (point-at-bol) t)
                (if (< pos-inst-start (+ 2 (current-column)))
                    (setq pos-inst-start (+ 2 (current-column))))
                ;; (setq pos-parent-end (my-verilog-search-pair-end-position left-pair right-pair))
                (setq length-current (my-verilog-port-max-length-by-search-parent left-pair right-pair))
                (if (< port-length length-current)
                    (setq port-length length-current))
                ))))
      (setq port-length (+ (- my-verilog-min-spc-for-align 1) port-length))
      ;; (message-box "port column: %d\ninst column: %d" port-length pos-inst-start)
      ;; align all inst entity's left-pair
      ;; inst-alist keeps the inst position before align inst, but when this function is ongoing,
      ;; all the position shuold be update real time
      ;; so scan all inst entity again
      (setq inst-index 1)
      (dolist (entity inst-alist) ;; indent to position
        (let ((param-entiy)
              (end-pattern)
              (inst-name (car (split-string (car entity))))
              (inst-type (cdr (split-string (car entity))))
              (updated-alist '())
              (inst-pos))
          ;; (debug)
          (message "indent port entity (%2d/%2d) : %s %s" inst-index inst-count inst-type inst-name)
          (setq inst-index (+ 1 inst-index))

          ;; (save-excursion
            (goto-char (point-min))
            (setq updated-alist (verilog-imenu-create-find-instances-or-modports (point-max)))
            ;; )
          (setq inst-pos (cdr (assoc (car entity) updated-alist)))
          (if (not (string= (car inst-type) "<module>"))
              (progn
                (goto-char inst-pos)
                (beginning-of-line)
                ;; parameter list
                (if (verilog-re-search-forward "\\s-?+#(" (point-at-eol) t)
                    (progn
                      (replace-match "#(" t t)
                      (backward-char 2) ;; indent "#(" to `pos-inst-start'
                      (insert (make-string (- (- pos-inst-start 1) (current-column)) ? ))
                      ;; (next-line)
                      ;; (my-verilog-indent-in-pair left-pair right-pair)
                      ;; goto end of parameter list
                      (my-verilog-indent-inst-signal-with-offset-by-search-parent port-length left-pair right-pair pos-inst-start)
                      ;; (goto-char (my-verilog-search-pair-end-position left-pair right-pair))
                      (verilog-re-search-forward "(" nil t)))
                (beginning-of-line)
                (verilog-re-search-forward "\\s-?+(" (point-at-eol) t)
                (replace-match "(" t t)
                (backward-char)
                (insert (make-string (- pos-inst-start (current-column)) ? ))
                (my-verilog-indent-inst-signal-with-offset-by-search-parent port-length left-pair right-pair pos-inst-start)
                ))))
      )))


(defun my-verilog-search-pair-end-position (left-pair right-pair)
    "Return the position of matched right-pair"
    (save-excursion
    ;; (beginning-of-line)
    (verilog-re-search-forward left-pair nil t)
    (let ((depth 1))
      (while (progn
               (verilog-re-search-forward (concat "\\([" left-pair right-pair "]\\)") nil t)
               (if (string= (verilog-match-string 1) left-pair)
                   (setq depth (1+ depth))
                 (setq depth (1- depth)))
               (> depth 0)))
      (backward-char)
      (point))))

(defun my-verilog-indent-in-pair (left-pair right-pair)
  "indent inside region LEFT-PAIR and RIGHT-PAIR"
  (save-excursion
    (beginning-of-line)
    (verilog-re-search-forward left-pair nil t)
    (let ((depth 1))
      (while (progn
               (while (not (verilog-re-search-forward (concat "\\([" left-pair right-pair "]\\)") (point-at-eol) t))
                 (next-line)
                 (beginning-of-line)
                 (electric-verilog-tab)
                 )
               (if (string= (verilog-match-string 1) left-pair)
                   (progn
                     (setq depth (1+ depth))
                     (electric-verilog-tab))
                 (setq depth (1- depth)))
               (> depth 0)))
      (backward-char)
      (point))))


(defun my-verilog-port-end-max-column-by-search-parent (left-pair right-pair)
  "Return max column of ports' name end inside LEFT-PARENT and RIGHT-PARENT

before call `my-verilog-port-end-max-column-by-search-parent', the cursor should at left side of LEFT-PARENT

;;    inst (          ;; \"(\": inital depth to  1, after \"(\" should be 1
;;    .a (sa),        ;; \"s\": depth inc
rease to 2, \")\": depth decrease to 1
;;    .b (sb[(1+2):0]);; \"s\": depth increase to 2, 2nd \"(\": depth incre to 3; 1st \")\", decrease to 2; 3rd \")\": decrease to 1
;;    )               ;; \")\": decrease to 0
"
  (save-excursion
    (let ((column 0) (depth 1))
      (verilog-re-search-forward "(" nil t)
      (while  (progn
                (verilog-re-search-forward (concat "\\([" left-pair  right-pair "]\\)") nil t)
                (if (string= (verilog-match-string 1) left-pair)
                    (setq depth (1+ depth))
                  (setq depth (1- depth)))

                (if (and (string= (verilog-match-string 1) left-pair)
                         (= depth 2))
                    (progn
                      (backward-char)
                      (skip-chars-backward " \t")
                      (backward-char)
                      (if (< column (current-column))
                          (setq column (current-column)))
                      ;; goto inside of parent: .port(port-signal)
                      (verilog-re-search-forward left-pair nil t)))
                (> depth 0)))
      column )))

(defun my-verilog-port-max-length-by-search-parent (left-pair right-pair)
  "Return max length of ports' name inside LEFT-PARENT and RIGHT-PARENT

before call `my-verilog-port-end-max-length-by-search-parent', the cursor should at left side of LEFT-PARENT

;;    inst (          ;; \"(\": inital depth to  1, after \"(\" should be 1
;;    .a (sa),        ;; \"s\": depth inc
rease to 2, \")\": depth decrease to 1
;;    .b (sb[(1+2):0]);; \"s\": depth increase to 2, 2nd \"(\": depth incre to 3; 1st \")\", decrease to 2; 3rd \")\": decrease to 1
;;    )               ;; \")\": decrease to 0
"
  (save-excursion
    (let ((length 0) (depth 1)
          (current-length)(port-st) (port-end))
      (verilog-re-search-forward "(" nil t)
      (while  (progn
                (verilog-re-search-forward (concat "\\([" left-pair  right-pair "]\\)") nil t)
                (if (string= (verilog-match-string 1) left-pair)
                    (setq depth (1+ depth))
                  (setq depth (1- depth)))

                (if (and (string= (verilog-match-string 1) left-pair)
                         (= depth 2))
                    (progn
                      (backward-char)
                      (skip-chars-backward " \t")
                      (setq current-length (* -1 (skip-chars-backward "[a-zA-Z0-9_]")));; skip back, give a negative number
                      (if (< length current-length)
                          (setq length current-length))
                      ;; goto inside of parent: .port(port-signal)
                      (verilog-re-search-forward left-pair nil t)))
                (> depth 0)))
      length )))

(defun my-verilog-indent-inst-port-to-column-by-search-parent (column left-pair right-pair)
  "indent all left-pair of signals to COLUMN, search boundry like `my-verilog-port-end-max-column-by-search-parent'"
  (save-excursion
    (let ((depth 1))
      (verilog-re-search-forward "(" nil t)
      (while  (progn
                (verilog-re-search-forward (concat "\\([" left-pair right-pair "]\\)") nil t)
                (if (string= (verilog-match-string 1) left-pair)
                    (setq depth (1+ depth))
                  (setq depth (1- depth)))

                (if (and (string= (verilog-match-string 1) left-pair)
                         (= depth 2))
                    (progn
                      (beginning-of-line)
                      (verilog-re-search-forward "(" nil t)
                      (beginning-of-line)
                      (if (verilog-re-search-forward "\\s-+(" (point-at-eol) t)
                          (replace-match "(" t t)
                        (verilog-re-search-forward "(" nil t))
                      (backward-char)
                      (while (< (current-column) column)
                        (insert " "))
                      ;; goto inside of parent: .port(port-signal)
                      (verilog-re-search-forward left-pair nil t)))
                (> depth 0)))
      )))

(defun my-verilog-indent-inst-signal-with-offset-by-search-parent (offset left-pair right-pair port-st-pos)
  "indent all left-pair of signals by offset, search boundry like `my-verilog-port-end-max-column-by-search-parent'"
  (let ((depth 1)
        (char )
        )
    (verilog-re-search-forward left-pair nil t)
    (while  (progn
              (verilog-re-search-forward (concat "\\([" left-pair  right-pair "]\\)") nil t)
              (setq char (verilog-match-string 1))
              (if (string= char left-pair)
                  (setq depth (1+ depth))
                (setq depth (1- depth)))

              (if (and (string= char left-pair)
                       (= depth 2))
                  (progn
                    (beginning-of-line)
                    (verilog-re-search-forward "^\\s-?+" nil t)
                    (replace-match (make-string (+ 1 port-st-pos) ? ) t t)
                    ;; (verilog-re-search-forward "(" nil t)
                    ;; (beginning-of-line)
                    (if (verilog-re-search-forward "\\s-+(" (point-at-eol) t)
                        (replace-match "(" t t)
                      (verilog-re-search-forward "(" nil t))
                    (backward-char)
                    ;; (while (< (current-column) offset)
                    ;;   (insert " "))
                    (insert (make-string (- (+ 1 offset) ;; offset = max_length, so insert at least one space
                                            ;; return length of current port
                                            (progn (skip-chars-backward "[a-zA-Z0-9_]")
                                                   (skip-chars-forward "[a-zA-Z0-9_]")))
                                         ? )) ;; copy " " by offset times
                    ;; goto inside of parent: .port(port-signal)
                    (verilog-re-search-forward left-pair nil t)))
              (> depth 0)))
    ))

(defun my-verilog-format-inst-port-to-single-line(inst-alist)
  "format inst's port list into single line"
(save-excursion
(let((isparamed)
          (param-left-parent-pos) (param-right-parent-pos)
          (inst-left-parent-pos) (inst-right-parent-pos))
      )))

(defcustom my-verilog-update-header-before-save t
  "True: update file header of verilog-mode"
  :group 'verilog-mode
  :type 'boolean)
(put 'my-verilog-update-header-before-save 'safe-local-variable 'integerp)

(defun my-update-last-update-stamp ()
  "update \"Last Update:\""
  (when my-verilog-update-header-before-save
    (save-excursion
      (goto-line 1)
      (if (buffer-modified-p)
          (progn
            (if (search-forward "Last Update : " nil t)
                (progn
                  (delete-region (point) (point-at-eol))
                  (verilog-insert-time))
              (message "Can't find the position to update \"Last Updated timing\""))
            (if (search-forward "Module Name : " nil t)
                (progn
                  (delete-region (point) (point-at-eol))
                  (insert (file-name-base (or (buffer-file-name) (buffer-name)))))
              (message "Can't find the position to update \"Module Name\""))
            (if (search-forward "Project Name: " nil t)
                (progn
                  (delete-region (point) (point-at-eol))
                  (insert  (projectile-project-name)))
              (message "Can't find the position to update \"Project Name\""))
            (if (search-forward "Engineer    : " nil t)
                (progn
                  (delete-region (point) (point-at-eol))
                  (insert (user-full-name))
                  (insert "<" (user-login-name) "@" (system-name) ">"))
              (message "Can't find the position to update \"Engineer\""))
            )))))

(defcustom my-verilog-auto-insert-header t
  " when it's value is t, emacs will execute `verilog-header' when open an empty buffer with minor mode `my-verilog',
to turn off it globally by:
(customize-set-variable 'my-verilog-auto-insert-header nil)
or add following line into end of buffer:

# Local \Variables:
# eval:(custom-set-variables '(my-verilog-auto-insert-header nil))
# End:
"
  :group 'verilog-mode)

(defcustom my-verilog-auto-readonly t
  "make buffer readonly if user-login-name not equal to Engineer of file header"
  :group 'verilog-mode)

(defun my-verilog-readonly ()
  "set buffer to read-only if Engineer field of header not equal to `uer-login-name' "
  (interactive)
  (and
   (not (string> (buffer-name) "timemachine:" ))
   (save-excursion
     (goto-line 1)
     (search-forward "Engineer    : " nil t)
     (forward-char)
     (let ((system-user (user-login-name))
           (file-author (symbol-at-point)))
       (and (not (string= system-user file-author))
            (require 'git-timemachine)
            (read-only-mode 1)
            )
       ))))

(defun my-verilog-indent/hs ()
  "work around `electric-verilog-tab', indent or hide/show fring.
If `buffer-read-only' is non-nil, execute `my-hideshowvis-fringe'.
If `electric-verilog-tab' don't change position, execute `my-hideshowvis-fringe'.
"
  (interactive)
  (if (or buffer-read-only
          (let* ((old-position (point))
                 (new-position (progn (electric-verilog-tab)
                                      (point))))
            (= old-position new-position))
          )
      (my-hideshowvis-fringe)))

(defun my-verilog-forward-close-endif ()
  "Find the close endif that match the current point.
Ignore other close parenthesis with matching open parens."
  (let ((parens 1))
    (while (> parens 0)
      (unless (verilog-re-search-forward-quick "\\(`ifdef\\|`ifndef\\|`endif\\)" nil t)
	(error "%s: Mismatching: `endif" (verilog-point-text)))
      (cond ((string= (verilog-match-string 1) "`ifdef" )
	     (setq parens (1+ parens)))
            ((string= (verilog-match-string 1) "`ifndef" )
	     (setq parens (1+ parens)))
            ((string= (verilog-match-string 1) "`endif" )
	     (setq parens (1- parens)))))))

(defun my-verilog-parse-macro-in-portlist ()
  "Return a  port list, contain keywords of ifdef ifndef else elsif endif

a module declear can be:
-----------
module suba(
`ifdef a,b,
`else d,e
`endif
f,g)
------------
or
------------
module subb(
`ifdef
input [MAX:MIN]a,b,
`else
output d,e,
`endif
output f,g)
`skip-syntax-forward \"'

- 寻找端口列表括号的结束点(verilog-forward-close-paren)，确定搜索范围
  - 搜索 ifdef ifndef
    - 搜索匹配的 endif
- 拿到一个符号后，需要判断下一个字符： (verilog-re-search-forward \"[^\s-]\" nil t)
  - 如果是 [: 端口定义放在端口列表中的语法结构，寻找匹配的 ] 的后面的符号
  - 如果是逗号，逗号前一个符号为端口名
  - 如果是条件宏，则前一个符号为端口名

"
  ;; (verilog-re-search-forward "\\(`ifdef\\)\\|\\(`ifndef\\)\\|\\(`endif\\)" nil t)
  ;; Find the file and point at which MODULE is defined: /usr/local/share/emacs/26.3/lisp/progmodes/verilog-mode.el.gz:11796
  ;;
  ;; (while (<= port-end (point))
  ;;    (let macro-end *)
  ;;    (goto-back-point)
  ;;    (while (search-forward \\(Ifdef\\|ifndef\\) to port-end )
  ;;      (let macro-matched-end *)
  ;;      (goto-back-point)
  ;;      (return-array-condMacro-port macro-matched-end)
  ;;    )
  ;; )
  )

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
  (define-key verilog-mode-map "\t" 'my-verilog-indent/hs)
  (if (and my-verilog-auto-insert-header
           (= (point-max) 1))
      (if (and
           (not (string> (buffer-name) "timemachine:" ))
           (yes-or-no-p "Buffer is empty, let's insert verilog-header?"))
          (verilog-header t)))
  (if my-verilog-auto-readonly
      (my-verilog-readonly))
  )
