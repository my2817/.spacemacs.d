(defun wttr/prepend-to-exec-path (path)
  "prepand the path to the emacs intenral `exec-path' and \"PATH\" env variable.
    Return the updated `exec-path'"
  (setenv "PATH" (concat (expand-file-name path)
                         path-separator
                         (getenv "PATH")))
  (setq exec-path
        (cons (expand-file-name path)
              exec-path)))
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
