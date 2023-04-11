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

Or if you want to pull stuff using ruby:

```ruby
  require 'open3'

  # Run the "lpass show" command to retrieve the Instapaper API key and username
  output, status = Open3.capture2('lpass', 'show', 'Instapaper API Key')
  if status.success?
    # Extract the API key and username from the output
    api_key = output.match(/Consumer ID: (.+)/)[1]
    api_secret = output.match(/Consumer Secret: (.+)/)[1] 
  end
```

## References

1. https://github.com/lastpass/lastpass-cli
2. https://support.lastpass.com/help/use-the-lastpass-command-line-application-lp040011