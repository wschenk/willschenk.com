---
title: "Developing React Inside Docker"
subtitle: "Clean up after your mess"
tags:
  - javascript
  - howto
  - docker
date: "2019-07-14"
draft: true
---

Can we build a node application without installing node locally?  We sure can!  Lets walk through the process.

First make sure that docker is installed.  This is handy if you are working on a remote server for example.

## Bootstrap

Then lets start building out the `Dockerfile` that we will use.

1. `mkdir testapp`
2. `cd testapp`
3. Create `Dockerfile.bootstrap`:

[`Dockerfile.bootstrap`](Dockerfile.bootstrap`)
{{% code file="articles/2019/developing_react_inside_docker" language="Dockerfile" %}}

This is going to download the `node:11` package, run `create-react-app` and then copy the generated code into the `/app` folder inside of the container.  Lets build this and start up the container, mapping the local directory into `/app`.  We are discarding the downloaded `node-modules` for now, we will import them later.

```
docker build -f Dockerfile.bootstrap . -t devimage && docker run -it --rm -v ${PWD}:/app devimage
```

Once this is done, you should see a basic `create-react-app` generated folder in your local directory.

```
$ ls -1
Dockerfile
README.md
exec.sh
package.json
public
src
yarn.lock
```

## Setting up for development

Create a `.dockerignore` file:

```
node_modules
.git
start.sh
```

Now lets edit the `Dockerfile.development` to run the app itself out of the local directory:

```
FROM node:11 as build
MAINTAINER Will Schenk <wschenk@gmail.com>

WORKDIR /app

COPY package.json yarn.lock /app/

RUN yarn install

EXPOSE 3000

CMD bash
```

1. First we copy over package.json and yarn.lock into /app.  If these files ever change, we will rerun all of the following steps when building the container.
2. Run `yarn install` to install the local `node_modules`
3. Expose port `3000` which is the development server port.

And create a script that will build and start up the container easily, called `start.sh`

```bash
#!/usr/bin/env bash

docker build . -f Dockerfile.development -t devimage && docker run -it --rm -v ${PWD}:/app -v devimage_nodemodules:/app/node_modules --network host devimage $@
```

Now lets fire up a development server using

```bash
$ bash start.sh yarn start
```

Once its running, start up the the development server by running `yarn start` inside of the container and opening up http://localhost:3000


Here we may need to change user permissions back to our user

```
$ sudo chown $USER:$USER -R .
```
