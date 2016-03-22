(defun wttr/prepend-to-exec-path (path)  
  "prepand the path to the emacs intenral `exec-path' and \"PATH\" env variable.  
    Return the updated `exec-path'"  
  (setenv "PATH" (concat (expand-file-name path)  
                         path-separator  
                         (getenv "PATH")))  
  (setq exec-path  
        (cons (expand-file-name path)  
              exec-path)))  
(mapc #'wttr/prepend-to-exec-path  
      (reverse   
       '("D:/Perl/bin"
         "D:/EDA/graphviz-2.38/bin"
         "D:/cygwin/bin"
         "D:/EDA/modeltech64_10.2c/win64"
         ))) 
