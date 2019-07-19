---
title: Beginning Emacs for JavaScript Development
subtitle: Rediscovering emacs
tags:
  - emacs
  - javascript
  - floss
date: 2019-07-16
draft: true
---

I've dipped back into the world of Free Software, a place that I used to know very well and it's now even better than I remember it.  I love the package manager!  I've started using emacs again -- a lot of the muscle memory is still there.  Lets look at how to setup emacs to do JavaScript development.

First install emacs.  You need emacs > 25 to use magit, so its probable that you'll need to upgrade the emacs in your system.  If you are using linux I'll let you sort that out directly, but below are the instructions for OSX.

## Using homebrew to install emacs on OSX

```bash
$ brew cask install emacs
```

If you are on OSX you'll also need to install some certs, and defined on [in this wonderful walkthrough](https://blog.vifortech.com/posts/emacs-tls-fix/)

```bash
$ brew install libressl
```

And then in `.emacs` add:

```elisp
(require 'gnutls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")
```

### Changing the capslock key to control

Go into `System Preferences...` -> `Keyboard` and select `Modifier Keys`.  Switch caps lock key to be control.  Totally worth it, but you still need to do a META dance with the options key, instead of the more obvious command key.

## Adding a new package registry

`M-x customize` and search for `package-archives`.  Insert a new one and add `https://stable.melpa.org/packages/`.  You can paste into emacs using `C-y`

Then `Apply and Save`

Refresh the package list using `M-x refresh-package-contents`

## Install rjsx-mode and xref-js2

I mainly do react development on the front end, and the `rjsx-mode` depends on the `js2-mode` which is pretty great.  We'll install this and `xref-js2` to help navigate through files better.

`M-x package-install` then `rjsx-mode`

`M-x package-install` then `xref-js2`

And add to your `~/.emacs`

```elisp
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
```

You can use the mouse to select -- or `C-space` to select the start of the region and move the pointer around. Once you have the region selected do `M-x eval-buffer` to load the config in place.

## Install `web-mode`

`M-x package-install` and enter in `web-mode`

## Install `magit`

`M-x package-install RET magit`.

Inside of `.emacs` add:

```elisp
(global-set-key (kbd "C-x g") 'magit-status)
```






## References

1. https://www.masteringemacs.org/
1 .https://blog.vifortech.com/posts/emacs-tls-fix/
2. https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
3. https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html
4. https://github.com/felipeochoa/rjsx-mode
