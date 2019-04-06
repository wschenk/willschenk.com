---
title: Easy scraping with httpie and jq
subtitle: Command line scripting
tags:
  - howto
date: 2019-04-01
draft: true
---

Linux:

```
sudo apt-get install httpie jq
```

OSX

```
brew install httpie
brew install jq
```

http https://api.github.com/users/wschenk/starred | jq '.[] | {name: .name, full_name: .full_name,  description: .description, starred: .updated_at, html_url: .html_url} '
