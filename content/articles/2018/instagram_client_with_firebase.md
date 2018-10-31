---
title: "Instagram Client with firebase"
subtitle: "no servers for this guy"
date: 2018-10-30
draft: true
tags:
  - howto
  - firebase
---

Getting a token from here:

http://instagram.pixelunion.net/


Firebase works best on node 6.

If you don't have nvm installed yet, do that.

$ nvm install 6
$ nvm use 6

Create a react app

$ npx create-react-app honey

$ cd honey



go to https://console.firebase.google.com  and create a project




$ npm install -g firebase-tools

$ firebase login

$ firebase init

select functions

select your project

javascript

yes to eslint

no to install dependancies

cd functions

npm install --save node-instagram


Create instagram app
https://www.instagram.com/developer/clients/register/
