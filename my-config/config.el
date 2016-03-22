(setq frame-title-format
'("GNU/Emacs - [ " (buffer-file-name "%f \]"
                                     (dired-directory dired-directory "%b \]"))))
(global-hl-line-mode -1)

(add-hook 'verilog-mode-hook 'global-auto-complete-mode)
(add-to-list 'auto-mode-alist '("\\.log\\'" . compilation-mode))
