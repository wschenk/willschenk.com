---
title: "Implementing the OAuth flow for JAM Stacks and static sites"
subtitle: "just a tiny bit of functional glue"
date: 2018-11-01
draft: true
tags:
  - howto
  - static_sites
  - functions
---

Most of the serverless platforms have their own forms of authentication, but it might not support the specific service that you are looking to use.  Lets go through how we can build a react single page app, hosting on firebase, that talks to the unsplash service directly.  It will be hosted on firebase stoage, and with a tiny bit of firebase functions to tie it together.

## How oauth works

<img src="oauth.sequence.svg" alt="sequence flow" class="img-fluid"/>

So simple right?
