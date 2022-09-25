(eval-when-compile
  (require 'cl))
(require 'info)
(require 'tree-widget)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))


(defvar widget-demo-form nil
  "A table for lookup widget created in current buffer.")

(defvar widget-demo-buffer-name "*Creat MRs*"
  "*Name of the widget demo buffer")


(defun widget-demo-form-create (id widget)
  (if (assoc id widget-demo-form)
      (error "identifier %S is used!" id)
    (push (cons id widget) widget-demo-form)))

(defun my-widget-demo ()
  "Show widget demo."
  (interactive)
  (switch-to-buffer "*Create MR*")
  (kill-all-local-variables)
  (make-local-variable 'widget-example-repeat)
  (make-local-variable 'widget-demo-form)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  (widget-insert "Create MR:\n\n")

  (widget-demo-form-create
   'mr-title
   (widget-create 'editable-field
                  :size 13
                  :format "Title: %v " ; Text after the field!
                  "fuck"))

  (widget-insert "\n")
  (widget-demo-form-create
   'mr-source
   (widget-create 'menu-choice
                  :tag "Source-Branch"
                  :value "master"
                  ;; (mapcar (lambda (b)
                  ;;           (list 'item :tag b :value b))
                  ;;         (magit-list-local-branch-names))
                  ;; '(item :tag "aaa" :value "aaa")
                  (list 'item :tag (format "%s" "mater") :value "master")
                  '(choice-item :tag "bbb"  "bbbc" "cc")
                  ;; '(item :tag "ccc" :value "ccc")
                  ))
  (use-local-map widget-keymap)
  (widget-setup)
  ;; (widget-value  (assoc-default 'mr-title widget-demo-form) )
  )
