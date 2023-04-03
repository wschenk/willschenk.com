---
title: Getting firebase and grpc working under termux
subtitle: tilting at windmills
date: 2017-12-10T17:36:17.928Z
tags:
  - chromebook
aliases:
  - "/articles/2017/getting_firebase_and_grpc_working_under_termux"
---

`firebase` depends on `grpc`, which isn’t set to build right on the Chromebook. This is because `node-pre-gyp`, which is used to download precompiled binaries, is not setup correctly. In then uses `node-gyp` to compile the C++ bindings directly, which is also not setup correctly.

We are going to adjust the settings of node-gyp to make it compile things right under `termux`, and then we are going to tweak the `grpc` package itself in our local cache to make it build correctly.

This took way to long to sort out.

### Installing the Basics

1. `pkg install nodejs python2 make clang pkg-config`
2. `termux-chroot`
3. `cd /bin; ln -s python2 python`
4. `mkdir /tmp/t; cd /tmp/t`
5. `npm install -g yarn`

### Start the grpc install

1. `yarn add grpc`
2. This should take a long time but then fail because `-zdefs` is not a valid option.

### Update node-gyp settings

First we update node-gyp, as [talked about here](http://blog.akehir.com/2017/05/building-node-sass-libsass-python.html).

1. `cd ~/. node-gyp/8.9.3/include/node` (or whatever version you have)
2. Update `common.gpyi` to change references of `-fPIE` to `-fPIC`, and get rid of `-pie`

### Update the grpc npm package in the yarn cache

1. `cd ~/.cache/yarn/v1/npm-grpc` (tab tab to find the actual version)
2. open up `bindings.gyp` and remove the line ‘`-zdefs`’ which for me is line 941
3. Also in `deps/grpc/bindings.gyp` remove `-zdefs` again. Not sure where this is coming from but `clang` doesn’t like it.

Note that every time there’s another version of grpc released, you’ll need to update that version in your yarn cache!

### Try to add again

1. `cd /tmp/t`
2. `rm -rf node_modules/grpc`
3. `yarn add grpc`

And there you go. Does anyone have a better fix that they could share?
