---
title: Guile and emacs
subtitle: Makes sense
tags:
  - howto
  - emacs
  - guile
date: "2020-03-04"
draft: true
---

## Install guile

The debian that I'm on only has guile-2.0 and Geiser needs at least 2.3.  

```bash
sudo apt-get install libgmp10-dev libtool libunistring-dev
cd $(mktemp -d)
wget https://ftp.gnu.org/gnu/guile/guile-3.0.0.tar.gz
tar xzf guile-3.0.0.tar.gz
cd guile-3.0.0
./configure
make

    

`M-x package-install RET paredit RET`
`M-x package-install RET geiser RET`


`M-x run-geiser`
