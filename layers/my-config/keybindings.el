;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-<SPC>") 'avy-goto-char)
(spacemacs/set-leader-keys "hh" 'highlight-symbol-at-point)
(spacemacs/set-leader-keys "hH" 'highlight-symbol-remove-all)
;; (spacemacs/set-leader-keys "g C-s" 'svn-global-keymap)
(spacemacs/set-leader-keys "ms" 'sos-op-on-file)
(spacemacs/set-leader-keys "md" 'ediff-current-file)
(spacemacs/set-leader-keys "mc" 'smex-major-mode-commands)
(spacemacs/set-leader-keys "mul" 'my-copy-file-line-to-clipboard)
(spacemacs/set-leader-keys "mue" 'toggle-company-english-helper)
(spacemacs/set-leader-keys "muh" 'my-hex2dec)
(spacemacs/set-leader-keys "mud" 'my-dec2hex)

(spacemacs/set-leader-keys "aou" 'org-projectile/update-agenda-files)
(spacemacs/set-leader-keys "Dfm" 'ora-ediff-files)
(spacemacs/set-leader-keys "gg" 'magit-gerrit-popup)
(spacemacs/set-leader-keys "m[" 'awesome-pair-wrap-bracket )
(spacemacs/set-leader-keys "m(" 'awesome-pair-wrap-round )
(spacemacs/set-leader-keys "m{" 'awesome-pair-wrap-curly )
(spacemacs/set-leader-keys "m\"" 'awesome-pair-wrap-double-quote)
(spacemacs/set-leader-keys "mU" 'awesome-pair-unwrap)
(spacemacs/set-leader-keys "m<" 'awesome-pair-jump-left)
(spacemacs/set-leader-keys "m>" 'awesome-pair-jump-right)
