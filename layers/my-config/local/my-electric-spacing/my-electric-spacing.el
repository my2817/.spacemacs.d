(defadvice electric-spacing-. (around my-electric-spacing-.)
  "my advice function for electric-spacing-."
  (cond ((and electric-spacing-double-space-docs
              (electric-spacing-document?))
         (electric-spacing-insert "." 'after)
         (insert " "))
        ((or (looking-back "[0-9]")
             (string= major-mode "verilog-mode")
             (or (and c-buffer-is-cc-mode
                      (looking-back "[a-z]"))
                 (and
                  (derived-mode-p 'python-mode 'ruby-mode)
                  (looking-back "[a-z\)]"))
                 (and
                  (derived-mode-p 'js-mode 'js2-mode)
                  (looking-back "[a-z\)$]"))))
         (insert "."))
        ((derived-mode-p 'cperl-mode 'perl-mode 'ruby-mode)
         ;; Check for the .. range operator
         (if (looking-back ".")
             (insert ".")
           (insert " . ")))
        (t
         (electric-spacing-insert "." 'after)
         (insert " ")))
  )
(ad-activate 'electric-spacing-.)
(defadvice electric-spacing-insert-1 (around my-electric-spacing-insert-1)
  "save-excursion before do 'indent-according-to-mode"
  (pcase only-where
    (`before (insert " " op))
    (`middle (insert op))
    (`after (insert op " "))
    (_
     (let ((begin? (bolp)))
       (unless (or (looking-back (regexp-opt
                                  (mapcar 'char-to-string
                                          (mapcar 'car electric-spacing-rules)))
                                 (line-beginning-position))
                   begin?)
         (insert " "))
       (insert op " ")
       (when begin?
         (save-excursion
           (indent-according-to-mode))))))
  )
(ad-activate 'electric-spacing-insert-1)

(defun electric-spacing-verilog-mode-\( ()
  (delete-horizontal-space)
  (if (looking-back (regexp-opt (mapcar 'char-to-string
                                        "@([{}])")))
      (electric-spacing-insert-1 "(" 'middle)
    (if (looking-back (regexp-opt '("if" "for" "while" "case" "casex" "casez")))
        (electric-spacing-insert-1 "(" 'before)
      (electric-spacing-insert-1 "(" 'middle)
      )))

(defun electric-spacing-verilog-mode-? ()
  (delete-horizontal-space)
  (electric-spacing-insert "?")
)

(defun electric-spacing-verilog-mode-: ()
  (delete-horizontal-space)
  (electric-spacing-insert ":")
  )

(defun electric-spacing-verilog-mode-@ ()
  (delete-horizontal-space)
  (electric-spacing-insert "@" 'before)
  )

(defun electric-spacing-plantuml-mode-@ ()
  (delete-horizontal-space)
  (electric-spacing-insert "@" 'middle)
  )

(defun electric-spacing-plantuml-mode-* ()
  (delete-horizontal-space)
  (electric-spacing-insert "@" 'middle)
  )

(defun electric-spacing-\( ()
  (delete-horizontal-space)
  (if (looking-back (regexp-opt (mapcar 'char-to-string
                                        "@([{}])")))
      (electric-spacing-insert-1 "(" 'middle)
    (if (looking-back (regexp-opt '("if" "for" "while" "case" "casex" "casez")))
        (electric-spacing-insert-1 "(" 'before)
      (electric-spacing-insert-1 "(" 'middle)
      )))

(defun electric-spacing-plantuml-mode-\( ()
  (delete-horizontal-space)
  (if (looking-back (regexp-opt (mapcar 'char-to-string
                                        "@([{}])")))
      (electric-spacing-insert-1 "(" 'middle)
    (if (looking-back (regexp-opt '("if" "for" "while" "case" "casex" "casez")))
        (electric-spacing-insert-1 "(" 'before)
      (electric-spacing-insert-1 "(" 'middle)
      )))

(defun electric-spacing-cc-mode-\( ()
  (delete-horizontal-space)
  (if (looking-back (regexp-opt (mapcar 'char-to-string
                                        "@([{}])")))
      (electric-spacing-insert-1 "(" 'middle)
    (if (looking-back (regexp-opt '("if" "for" "while" "case" "casex" "casez")))
        (electric-spacing-insert-1 "(" 'before)
      (electric-spacing-insert-1 "(" 'middle)
      )))

;;;###autoload
(define-minor-mode my-electric-spacing
  "a minor mode to customize `electric-spacing-mode'"
  :group 'electric-spacing
  :init-value nil
  ;; :lighter "CompanyVerilog"
  :global nil
  :after-hook nil

  )
