---
title: Using lastpass from CLI or script
subtitle: better than keeping files around
tags:
  - cli
  - lastpass
date: 2023-04-06
---

Using a password manager is great (though a troubling central point of failure).  We can
use command line tools to get the password from them rather than storing them in files which is much cleaner.

## `lpass` and login

On OSX:

```bash
brew install lastpass-cli
```

or Debian:

```bash
sudo apt-get install lastpass-cli
```

## Checking to see if you are logged in

```bash
if ! lpass status; then
  echo Please log in
  lpass login wschenk@gmail.com
fi
```

## Using it in a script

```bash
  POCKET_USER=`lpass show –password pocketcasts.com |awk '/Username/ { print $2}'`
  POCKET_PASS=`lpass show –password pocketcasts.com |awk '/Password/ { print $2}'`
```

## References

1. https://github.com/lastpass/lastpass-cli
2. https://support.lastpass.com/help/use-the-lastpass-command-line-application-lp040011