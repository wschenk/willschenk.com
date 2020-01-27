---
title: Installing dokku
subtitle: host your own heroku
tags:
  - howto
  - dokku
  - terraform
date: "2020-01-24"
draft: true
---

Dokku is a heroku like environment that you run on a single server that gives a lot of conviences that you get from Heroku. It's easy to deploy applications using `git push`, it manages the 12 factor app configuration, and lets you spin up and connect other servers like postgres and redis fairly easily.  Lets go through how to set this up.

1. We'll build off of the last post to manage the actual servers and dns with terraform


terraform init
terraform apply
bash setup.bash
open $(terraform output server_ip)
turn on virtualhosts and press finish setup


