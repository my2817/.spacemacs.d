
(require 'plantuml-mode)

(defadvice plantuml-indent-line (around my-plantuml-indent-line)
  ;; (defun plantuml-indent-line ()
  "Indent current line as plantuml code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        (indent-line-to 0)
      (let ((not-indented t) cur-indent var-indent)
        (if (looking-at my-plantuml-indent-regexp-end)
            (progn
              (save-excursion
                (forward-line -1)
                (if (looking-at my-plantuml-indent-regexp-start)
                    (setq cur-indent (current-indentation))
                  (setq cur-indent(- (current-indentation)
                                     plantuml-indent-offset))))
              (if (< cur-indent 0)
                  (setq cur-indent 0)))
          (save-excursion
            (while not-indented
              (forward-line -1)
              (cond
               ((looking-at my-plantuml-indent-regexp-start)
                (setq cur-indent (+ (current-indentation)
                                    plantuml-indent-offset)
                      not-indented nil))
               ((looking-at my-plantuml-indent-regexp-end)
                (setq cur-indent (current-indentation)
                      not-indented nil))
               ((progn (forward-line 1)
                       (setq var-indent
                             (looking-at my-plantul-indent-regexp-arrow))
                       (forward-line -1)
                       var-indent)
                (cond
                 ((> (setq var-indent
                           (string-match
                            (progn (string-match
                                    my-plantul-indent-regexp-arrow-1
                                    (current-line-string))
                                   (match-string-no-properties
                                    0
                                    (current-line-string)))
                            (current-line-string))) 0)
                  (setq cur-indent  var-indent
                        not-indented nil))))
               ((progn (forward-line 1)
                       (setq var-indent
                             (looking-at my-plantul-indent-regexp-arrow-2))
                       (forward-line -1)
                       var-indent)
                (cond
                 ('t
                   (let ((var-count 0) (var-flag t))
                     (while var-flag
                       (incf var-count)
                       (forward-line -1)
                       (cond ((bobp) (setq var-flag nil))
                             ((looking-at my-plantul-indent-regexp-arrow) nil)
                             ((looking-at "^\s+$") nil)
                             ((looking-at my-plantuml-indent-regexp-end) nil)
                             ((looking-at my-plantuml-indent-regexp-start) nil)
                             ('t (setq cur-indent (current-indentation)
                                       not-indented nil
                                       var-flag nil))))
                     (forward-line var-count)))))
               ((bobp) (setq not-indented nil))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0)))))
  (if (bolp)
      (end-of-line))
  )

;; TODO: go back to original, but need re-cal indent level after "repeat while..."
(ad-activate 'plantuml-indent-line)

(defconst plantuml-beg-block-re-ordered
  (concat "\\(\\<@startuml\\>\\)" ; 1 -> @enduml
          "\\|\\(\\<fork\s+again\\>$\\)"       ; 2 -> "fork again" or "end fork"
          "\\|\\(fork$\\)"               ; 3 -> "fork again" or "end fork"
          "\\|\\({start}\\)"            ; 4 -> "{end}"
          "\\|\\(\\<if\\>\\)"                 ; 5 -> "else" "elseif", "endif"
          "\\|\\(\\<elseif\\>\\)"             ; 6 -> "elseif" "else" "endif"
          "\\|\\(\\<else\\>\\)"               ; 7 -> "endif"
          "\\|\\(\\<while\\)"              ; 8 -> "endwhile"
          "\\|\\(\\<loop\\)"               ; 9 -> "end"
          "\\|\\(\\<alt\\)"                ; 10 -> "else" "end"
          "\\|\\(\\<repeat\s+\:\\)"             ; 11 -> "repeat while"
          "\\|\\(\\<note\\)"               ; 12 -> "end note", "note.*:" is signal line note, without "end note"
          ;; "\\|\\(?:.*\\)?\s*\\(?:[<>.*a-z-|]+\\)?\s*\\(?:\\[[a-zA-Z]+\\]\\)?\s+if"
          ;; "\\|note\s+\\(\\(?:\\(?:buttom" "\\|left" "\\|right" "\\|top\\)\\)\\)\\(?:\s+of\\)?"
          ))

(defun plantuml-forward-sexp-function (arg)
  "Move forward ARG sexps."
  ;; Used by hs-minor-mode
  (if (< arg 0)
      (plantuml-backward-sexp)
    (plantuml-forward-sexp)))

(setq plantuml-indent-level 3)

(defun plantuml-forward-sexp ()
  (let ((reg)
	(md 2)
	(st (point))
	(nest 'yes))
    (unless (looking-at "\\<")
      (forward-word-strictly -1))
    (cond
     ((save-excursion
        (goto-char st)
        (member (following-char) '(?\( ?\{ ?\[)))
      (goto-char st)
      (forward-sexp 1))
     ((looking-at plantuml-beg-block-re-ordered)
      (cond
       ((match-end 1)
        (setq reg "\\(@startuml\\>\\)\\|\\(@enduml\\)")
        (setq nest 'no))
       ((match-end 2)
        (setq reg "\\(\\<fork\s+again\\>\\|\\<fork\\>$\\)\\|\\(end\s+fork\\)"))
       ((match-end 3)
        (setq reg "\\(^\s?+fork$\\)\\|\\(\\<fork\s+again\\>\\|\\<end\s+fork\\>\\)"))
       ((match-end 4)
        (setq reg "\\({start}\\)\\|\\({end}\\)")
        (setq nest 'no))
       ((match-end 5)
        (setq reg "\\(if\\)\\|\\(\\<elseif\\>\\>\\|\\<else\\>\\|\\<endif\\>\\)"))
       ((match-end 6)
        (setq reg "\\(\\<elseif\\>\\|\\<if\\>\\)\\|\\(\\<elseif\\>\\>\\|\\<else\\>\\|\\<endif\\>\\)"))
       ((match-end 7)
        (setq reg "\\(\\<else\\>\\|\\<if\\>\\)\\|\\(\\<endif\\>\\)"))
       ((match-end 8)
        (setq reg "\\(while\\)\\|\\(endwhile\\)"))
       ((match-end 9)
        (setq reg "\\(loop\\)\\|\\(end\\)"))
       ((match-end 10)
        (setq reg "\\(alt\\)\\|\\(\\<else\\>\\|\\<end\\>\\)"))
       ((match-end 11)
        (setq reg "\\(repeat\s+\:\\)\\|\\(\\<repeat\s+while\\>\\)"))
       ((match-end 12)
        (setq reg "\\(note\\)\\|\\(end\s+note\\)")
        (setq nest 'no))
       )
      (if (and reg
	       (forward-word-strictly 1))
	  (catch 'skip
	    (if (eq nest 'yes)
		(let ((depth 1)
		      here)
		  (while (re-search-forward reg nil 'move)
		    (cond
		     ((and (or (match-end md)
                               (and (member (match-string-no-properties 1) '("else" "elseif" "fork again"))
                                    (= 1 depth)))
                           (or (and (member (match-string-no-properties 2) '("else" "elseif" "fork again"))
                                    (= 1 depth)) ;; stop at `else/`elsif which matching ifn?def (or `elsif with same depth)
                               (not (member (match-string-no-properties 2) '("else" "elseif" "fork again"))))) ; a closer in regular expression, so we are climbing out
		      (setq depth (1- depth))
		      (if (= 0 depth) ; we are out!
			  (throw 'skip 1)))
		     ((and (match-end 1) ; an opener in the r-e, so we are in deeper now
                           (not (member (match-string-no-properties 1) '("else" "elseif" "fork again"))))
		      (setq here (point)) ; remember where we started
		      (goto-char (match-beginning 1))
                      (progn ; it is a simple fork (or has nothing to do with fork)
			(goto-char here)
			(setq depth (1+ depth)))))))
	      (if (re-search-forward reg nil 'move)
		  (throw 'skip 1)))))))))




(setq my-plantuml-indent-regexp-end (concat "^[ \t]*\\(?:@enduml"
                                            "\\|end\s+fork"
                                            "\\|fork\s+again"
                                            "\\|end\s+note"
                                            "\\|endif"
                                            "\\|end"
                                            "\\|elseif"
                                            "\\|else"
                                            "\\|endwhile"
                                            "\\|repeat\s+while"
                                            ;; "\\|stop"
                                            "\\|}\\)"))
(setq my-plantuml-indent-regexp-start (concat "^[ \t]*\\(?:@startuml"
                                              "\\|fork\s+again"
                                              "\\|fork"
                                              ;; "\\|start" ;; may not balance, disable it
                                              "\\|if"
                                              "\\|elseif"
                                              "\\|else"
                                              "\\|while"
                                              "\\|loop"
                                              "\\|alt"
                                              "\\|repeat\s+:";; work with "repeat while *"
                                              "\\|\\(?:.*\\)?\s*\\(?:[<>.*a-z-|]+\\)?\s*\\(?:\\[[a-zA-Z]+\\]\\)?\s+if"
                                              "\\|note\s+over"
                                              "\\|note\s+\\(\\(?:\\(?:buttom"
                                              "\\|left"
                                              "\\|right"
                                              "\\|top\\)\\)\\)\\(?:\s+of\\)?"
                                              "\\|.*{\\)"))
(defvar my-plantul-indent-regexp-arrow (concat "^[ \t]*\\(?:\\(?:<"
                                               "\\|<|"
                                               "\\|o"
                                               "\\|\\*\\)\\(?:\\."
                                               "\\|-\\)\\(?:down"
                                               "\\|up"
                                               "\\|left"
                                               "\\|right\\)?\\(?:\\."
                                               "\\|-\\)"
                                               "\\|\\(?:-"
                                               "\\|\\.\\)\\(?:down"
                                               "\\|up"
                                               "\\|left"
                                               "\\|right\\)?\\(?:-"
                                               "\\|\\.\\)\\(?:>"
                                               "\\||>"
                                               "\\|\\*"
                                               "\\|o\\)\\)"))
(defvar my-plantul-indent-regexp-arrow-1 (concat "\\(?:\\(?:<"
                                                 "\\|<|"
                                                 "\\|o"
                                                 "\\|\\*\\)\\(?:\\."
                                                 "\\|-\\)\\(?:down"
                                                 "\\|up"
                                                 "\\|left"
                                                 "\\|right\\)?\\(?:\\."
                                                 "\\|-\\)"
                                                 "\\|\\(?:-"
                                                 "\\|\\.\\)\\(?:down"
                                                 "\\|up"
                                                 "\\|left"
                                                 "\\|right\\)?\\(?:-"
                                                 "\\|\\.\\)\\(?:>"
                                                 "\\||>"
                                                 "\\|\\*"
                                                 "\\|o\\)\\)"))
(defvar my-plantul-indent-regexp-arrow-2 (concat "^\s*.+\s+\\(?:\\(?:<"
                                                 "\\|<|"
                                                 "\\|o"
                                                 "\\|\\*\\)\\(?:\\."
                                                 "\\|-\\)\\(?:down"
                                                 "\\|up"
                                                 "\\|left"
                                                 "\\|right\\)?\\(?:\\."
                                                 "\\|-\\)"
                                                 "\\|\\(?:-"
                                                 "\\|\\.\\)\\(?:down"
                                                 "\\|up"
                                                 "\\|left"
                                                 "\\|right\\)?\\(?:-"
                                                 "\\|\\.\\)\\(?:>"
                                                 "\\||>"
                                                 "\\|\\*"
                                                 "\\|o\\)\\)"))

(defvar plantuml-indent-offset 3)

;;;###autoload
(define-minor-mode my-plantuml
  "a minor mode to handle company backend for verilog"
  :group 'plantuml
  :init-value nil
  :global nil
  :after-hook
  (if my-plantuml
      (progn
        (setq company-backends
              '(
                company-files
                company-dabbrev-code company-capf
                company-abbrev
                ))
        (make-local-variable 'company-dabbrev-code-ignore-case)
        (setq company-dabbrev-code-ignore-case t)
        (make-local-variable 'company-dabbrev-code-everywhere)
        (setq company-dabbrev-code-everywhere t)

        (setq hs-special-modes-alist (assq-delete-all 'plantuml-mode hs-special-modes-alist))
        (add-to-list 'hs-special-modes-alist
                     `(plantuml-mode ,(concat "\\(\\<@startuml\\>\\)"
                                              "\\|\\(\\<fork\\>$\\)"
                                              "\\|\\(\\<fork\s+again\\>$\\)"
                                              "\\|\\(\\<if\\>\\)"
                                              "\\|\\(\\<elseif\\>\\)"
                                              "\\|\\(\\<else\\>\\)"
                                              "\\|\\(\\<repeat\s+\:\\)"
                                              )
                                     ,(concat "\\(@enduml\\)"
                                              "\\|\\(\\<end\s+fork\\>\\)"
                                              "\\|\\(\\<fork\s+again\\>\\)"
                                              "\\|\\(\\<endif\\>\\)"
                                              "\\|\\(\\<elseif\\>\\)"
                                              "\\|\\(\\<repeat\s+while\\>\\)"
                                              )
                             nil plantuml-forward-sexp-function))
        (hs-minor-mode 1)
        )
    (setq company-dabbrev-code-ignore-case nil)
    ))
