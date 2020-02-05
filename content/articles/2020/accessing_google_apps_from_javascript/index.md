---
title: Accessing Google Suite from JavaScript
subtitle: who needs servers
tags:
  - howto
  - javascript
  - google suite
date: "2020-02-05"
draft: true
---

You need to be the adminstrator of your domain to set this up.

1. Be a Google Suite Admin
2. Create an app in the Google Developers Console
3. Enable API
4. Verify ownership of your domain
5. Whitelist the url

Let's get started.

## Be a Google Suite Admin

I'm assuming that you already are hosting email and docs on Google App Suite, and that you are an admin.

## Create an App

1. Go to the [Google Cloud Console](https://console.cloud.google.com/)
2. Sign in with your domain account
3. Create a new project, for example "majordomo".


1. Go to the [Google Admin console](https://admin.google.com/)
2. Hamburger > Security > App access Control
3. Make sure that "Trust internal, domain-owned apps box" is selected.


Sign in

https://support.google.com/a/answer/7281227

## References

1. https://support.google.com/a/answer/7281227

