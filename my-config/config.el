(setq frame-title-format
'("GNU/Emacs - [ " (buffer-file-name "%f \]"
                                     (dired-directory dired-directory "%b \]"))))
(global-hl-line-mode -1)
(defcustom graphviz-dot-mode-hook nil
  "Hook run after graphviz-dot-mode is loaded"
  :type 'hook
  :group 'graphviz-dot-mode)
(add-hook 'verilog-mode-hook 'global-auto-complete-mode)
(add-to-list 'auto-mode-alist '("\\.log\\'" . compilation-mode))
(global-set-key (kbd "C-<SPC>") 'avy-goto-char)
(add-hook 'graphviz-dot-mode-hook 'smartparens-mode)
(blink-cursor-mode)
(mapc #'wttr/prepend-to-exec-path
      (reverse
       '("D:/Perl/bin"
         "D:/EDA/graphviz-2.38/bin"
         "D:/cygwin/bin"
         "D:/EDA/modeltech64_10.2c/win64"
         "D:/EDA/Git-2.6.0-32-bit/PortableGit/bin"
         )))
