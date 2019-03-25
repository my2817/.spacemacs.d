# emacs
it's my config layer of spacemacs

# install
- get spacemacs by:

```
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

- get my configuration by:

```
git clone https://git.oschina.net/my2817/spacemacs-d ~/.spacemacs.d
```

or

```
git clone https://github.com/my2817/.spacemacs.d ~/.spacemacs.d
```

- some configuration:

Because of the GFW, the melpa source is changed to local file system in ~/.spacemacs.d/init.el by function "dotspacemacs/user-init", where the local source file is at "~/.emacs.d/.cache/elpa.emacs-china.org". And the local source file is synced by:

```
rsync -avzp rsync:elpa.emacs-china.org/elpa/ ~/.emacs.d/.cache/elpa.emacs-china.org
```

or you can disable local melpa source by change melpa-source from "local" to "emacs-china"
