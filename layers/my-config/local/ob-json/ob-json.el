;; write image of json by plantuml
;; copy from ob-plantuml, replace "plantuml" by "json-plantuml"
;;
;; (setq org-plantuml-jar-path
;;             (expand-file-name "~/.spacemacs.d/plantuml.jar"))
;;       (setq org-json-plantuml-jar-path
;;             (expand-file-name "~/.spacemacs.d/plantuml.jar"))
;; (org-babel-do-load-languages
;;        'org-babel-load-languages
;;        '((perl . t)
;;          (ruby . t)
;;          (shell . t)
;;          (dot . t)
;;          (js . t)
;;          (latex . t)
;;          (python . t)
;;          (emacs-lisp . t)
;;          (plantuml . t)
;;          (C . t)
;;          (awk . t)
;;          (ditaa . t)
;;          (json . t)
;;          ;; (mermaid . t)
;;          ))
;;

(require 'ob)

(defvar org-babel-default-header-args:json
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a plantuml source block.")

(defcustom org-json-plantuml-jar-path ""
  "Path to the plantuml.jar file."
  :group 'org-babel
  :version "24.1"
  :type 'string)

(defcustom org-json-plantuml-exec-mode 'jar
  "Method to use for PlantUML diagram generation.
`jar' means to use java together with the JAR.
The JAR can be configured via `org-json-plantuml-jar-path'.

`plantuml' means to use the PlantUML executable.
The executable can be configured via `org-json-plantuml-executable-path'.
You can also configure extra arguments via `org-json-plantuml-executable-args'."
  :group 'org-babel
  :package-version '(Org . "9.4")
  :type 'symbol
  :options '(jar plantuml))

(defcustom org-json-plantuml-executable-path "plantuml"
  "File name of the PlantUML executable."
  :group 'org-babel
  :package-version '(Org . "9.4")
  :type 'string)

(defcustom org-json-plantuml-executable-args (list "-headless")
  "The arguments passed to plantuml executable when executing PlantUML."
  :group 'org-babel
  :package-version '(Org . "9.4")
  :type '(repeat string))

(defcustom org-babel-json-plantuml-svg-text-to-path nil
  "When non-nil, export text in SVG images to paths using Inkscape."
  :group 'org-babel
  :package-version '(Org . "9.5")
  :type 'boolean)

;; TODO: dumplicate with ob-plantuml
(defun org-babel-variable-assignments:plantuml (params)
  "Return a list of PlantUML statements assigning the block's variables.
PARAMS is a property list of source block parameters, which may
contain multiple entries for the key `:var'.  `:var' entries in PARAMS
are expected to be scalar variables."
  (mapcar
   (lambda (pair)
     (format "!define %s %s"
	     (car pair)
	     (replace-regexp-in-string "\"" "" (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-json-plantuml-make-body (body params)
  "Return PlantUML input string.

BODY is the content of the source block and PARAMS is a property list
of source block parameters.  This function relies on the
`org-babel-expand-body:generic' function to extract `:var' entries
from PARAMS and on the `org-babel-variable-assignments:plantuml'
function to convert variables to PlantUML assignments.

If BODY does not contain @startXXX ... @endXXX clauses, @startuml
... @enduml will be added."
  (let ((full-body
	 (org-babel-expand-body:generic
	  body params (org-babel-variable-assignments:plantuml params))))
    (if (string-prefix-p "@startjson" body t) full-body
      (format "@startjson\n%s\n@endjson" full-body))))

(defun org-babel-execute:json (body params)
  "Execute a block of plantuml code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((out-file (or (cdr (assq :file params))
		       (error "PlantUML requires a \":file\" header argument")))
	 (cmdline (cdr (assq :cmdline params)))
	 (in-file (org-babel-temp-file "plantuml-"))
	 (java (or (cdr (assq :java params)) ""))
	 (executable (cond ((eq org-json-plantuml-exec-mode 'plantuml) org-json-plantuml-executable-path)
			   (t "java")))
	 (executable-args (cond ((eq org-json-plantuml-exec-mode 'plantuml) org-json-plantuml-executable-args)
				((string= "" org-json-plantuml-jar-path)
				 (error "`org-json-plantuml-jar-path' is not set"))
				((not (file-exists-p org-json-plantuml-jar-path))
				 (error "Could not find plantuml.jar at %s" org-json-plantuml-jar-path))
				(t (list java
					 "-jar"
					 (shell-quote-argument (expand-file-name org-json-plantuml-jar-path))))))
	 (full-body (org-babel-json-plantuml-make-body body params))
	 (cmd (mapconcat #'identity
			 (append
			  (list executable)
			  executable-args
			  (pcase (file-name-extension out-file)
			    ("png" '("-tpng"))
			    ("svg" '("-tsvg"))
			    ("eps" '("-teps"))
			    ("pdf" '("-tpdf"))
			    ("tex" '("-tlatex"))
			    ("vdx" '("-tvdx"))
			    ("xmi" '("-txmi"))
			    ("scxml" '("-tscxml"))
			    ("html" '("-thtml"))
			    ("txt" '("-ttxt"))
			    ("utxt" '("-utxt")))
			  (list
			   "-p"
			   cmdline
			   "<"
			   (org-babel-process-file-name in-file)
			   ">"
			   (org-babel-process-file-name out-file)))
			 " ")))
    (with-temp-file in-file (insert (concat "@starjson \n" full-body "\n@endjson")))
    (message "%s" cmd) (org-babel-eval cmd "")
    (if (and (string= (file-name-extension out-file) "svg")
             org-babel-json-plantuml-svg-text-to-path)
        (org-babel-eval (format "inkscape %s -T -l %s" out-file out-file) ""))
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:json (_session _params)
  "Return an error because plantuml does not support sessions."
  (error "Plantuml does not support sessions"))

(provide 'ob-json)

;;; ob-plantuml.el ends here
