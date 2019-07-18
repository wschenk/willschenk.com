---
title: Beginning Emacs for JavaScript Development
subtitle: Rediscovering emacs
tags:
  - emacs
  - javascript
date: 2019-07-16
draft: true
---

First install emails.  You need emacs > 25 to use magit, so I'll leave it up to you to figure out how to get the right version for your system.

## Adding a new package registry

`M-x customize` and search for `package-archives`.  Insert a new one and add `https://stable.melpa.org/packages/`.  You can paste into emacs using `C-y`

Then `Apply and Save`

Refresh the package list using `M-x refresh-package-contents`

## Install rjsx-mode and xref-js2

`M-x package-install` then `rjsx-mode`

`M-x package-install` then `xref-js2`

And add to your `~/.emacs`

```elisp
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
```

## Install `web-mode`

`M-x package-install` and enter in `web-mode`

Then enable web-mode for jsx files.  Open up `~/.emacs` and add the following sexp:

```elisp
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

```

Then `C-space` to select the start of the region, move the cursor to the end, and `M-x eval-region`.  This gives sytnax highlighting and other fun stuff for jsx files.





## References

1. https://www.masteringemacs.org/
2. https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
3. https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
4. https://github.com/felipeochoa/rjsx-mode
