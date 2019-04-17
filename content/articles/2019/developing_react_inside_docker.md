---
title: "Developing React Inside Docker"
subtitle: "Clean up after your mess"
tags:
  - javascript
  - howto
  - docker
date: "2019-04-17"
draft: true
---

Can we build a node application without installing node locally?  We sure can!  Lets walk through the process.

First make sure that docker is installed.  This is handy if you are working on a remote server for example.

## Project initialization

Then lets start building out the `Dockerfile` that we will use.

1. `mkdir testapp`
2. `cd testapp`
3. Create `Dockerfile`:

```
FROM node:11 as build
MAINTAINER Will Schenk <wschenk@gmail.com>

CMD bash
```

Now lets create a simple script that will help us run things, `exec.sh`


```bash
!#!/usr/bin/env bash

docker build . -t devimage && docker run -it --rm -v ${PWD}/src:/app/src --network host devimage
```

Lets run this command with `bash exec.sh`.  This will download the latest version of `node:11`, and build an image that will just immediately run `bash`.  We then run an interactive container `-it` that we will remove right afterwards `--rm` mounting the `src/`  directory to `/app/src` inside of the container.

Once we are inside of the container, we will

1. Generate the app using `create-react-app`
2. Discard `node-modules`
3. Copy all of the files back into the `/app` directory, which is mounted from outside of the container.
4. Exit out of the container

```
root@999415b7acf4:/# npx create-react-app testapp
root@999415b7acf4:/# rm -rf /testapp/node_modules
root@999415b7acf4:/# mv /testapp/* /app
root@999415b7acf4:/# exit
```

Here we may need to change user permissions back to our user

```
$ sudo chown $USER:$USER -R .
```

## Development setup

Create a `.dockerignore` file:

```
node_modules
.git
exec.sh
```


Now lets edit the `Dockerfile` to run the app itself out of the local directory:

```
FROM node:11 as build
MAINTAINER Will Schenk <wschenk@gmail.com>

WORKDIR /app

COPY package.json yarn.lock /app/

RUN yarn install

COPY . /app/

CMD bash
```
