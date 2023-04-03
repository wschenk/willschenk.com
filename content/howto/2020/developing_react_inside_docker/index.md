---
title: "Developing React Inside Docker"
subtitle: "Clean up after your mess"
tags:
  - javascript
  - docker
  - transient
date: "2020-06-16"
aliases:
  - "/articles/2020/developing_react_inside_docker"
---

Can we build a node application without installing node locally? We sure can! Lets walk through the process.

First make sure that docker is installed. This is handy if you are working on a remote server for example.

## Bootstrap

Then lets start building out the `Dockerfile` that we will use.

1. `mkdir testapp`
2. `cd testapp`
3. Create a `Dockerfile.initial` that has `node:14` in it.
4. Start up the container with

[`Dockerfile.initial`](Dockerfile.initial`)
{{< highlight "Dockerfile" >}}
{{% raw "Dockerfile.initial" %}}
{{< / highlight >}}

This is going to download the `node:14` docker image, and start a shell.

Lets build and start it a temporary container that has our new directory mounted in there and run the `npx create-react-app .` command, which will generate the new app.

```bash
docker build . -f Dockerfile.initial -t testapp
docker run -it --rm -v ${PWD}/testapp:/app  testapp npx create-react-app .
```

Once this is done, you should see a basic `create-react-app` generated folder in your local `testapp` directory. We need to adjust some permissions here since it's probably owned by root.

```bash
sudo chown -R ${USER} testapp
```

Revel in our generated node_modules!

```bash
$ ls -l testapp/
total 472
drwxr-xr-x 1 wschenk root  25562 Jun 16 06:08 node_modules
-rw-r--r-- 1 wschenk root    739 Jun 16 06:08 package.json
drwxr-xr-x 1 wschenk root    132 Jun 16 06:07 public
-rw-r--r-- 1 wschenk root   2884 Jun 16 06:07 README.md
drwxr-xr-x 1 wschenk root    156 Jun 16 06:07 src
-rw-r--r-- 1 wschenk root 471966 Jun 16 06:08 yarn.lock

```

I hate `node_modules` so lets delete it.

```bash
$ rm -rf node_modules
```

## Setting up for development

If you keep the `node_modules` directory around you may have some cross platform issues. `node_modules` directory may contain native code, which would be built against the libraries inside of the docker container. If you are running the same OS as what's inside of the container -- Linux -- then you can have these co-mingle, but if you are on OSX this is potentially a pain.

What we are going to do is use a docker volume to hide all of this away so we never need to see it and worry about it poluting our machine. Make sure you've deleted that terrible folder!

Lets create a `testapp/Dockerfile` to run the app itself out of the local directory:

```Dockerfile
FROM node:14

WORKDIR /app

COPY package.json yarn.lock /app/

RUN yarn install

EXPOSE 3000

CMD bash
```

1. First we copy over package.json and yarn.lock into /app. If these files ever change, we will rerun all of the following steps when building the container.
2. Run `yarn install` to install the local `node_modules`
3. Expose port `3000` which is the development server port.
4. We'll create a docker volume to hide the mess away.

And create a script that will build and start up the container easily, called `start.sh`

```bash
#!/usr/bin/env bash

docker build . -f Dockerfile -t testapp && \
docker run -it --rm -v ${PWD}:/app -v testapp_nodemodules:/app/node_modules --network host testapp $@
```

Now lets fire up a development server using

```bash
$ bash start.sh yarn start
```

And edit `testapp/src/App.js`, make a change, and revel in the automatic reloading!

## Adding a library

Kill the server with `C-c`. Lets go through the process of adding a library.

```bash
$ bash start.sh yarn add react-globe.gl
$ bash start.sh yarn start
```

In the first step, you see things get downloaded from the internet. If you look at your `package.json` file after it will be updated with the dependancy. The second time we run the `start.sh` script it will notice that `package.json` changed and rerun `yarn install`, but since we have mounted `node_modules` in a volume it doesn't need to pull anything more from the internet.

Lets edit our `App.js` file to use the new component to make sure that it works.

```jsx
import React from "react";
import Globe from "react-globe.gl";

import "./App.css";

function App() {
  // Gen random data
  const N = 300;
  const gData = [...Array(N).keys()].map(() => ({
    lat: (Math.random() - 0.5) * 180,
    lng: (Math.random() - 0.5) * 360,
    size: Math.random() / 3,
    color: ["red", "white", "blue", "green"][Math.round(Math.random() * 3)],
  }));

  return (
    <Globe
      globeImageUrl="//unpkg.com/three-globe/example/img/earth-night.jpg"
      pointsData={gData}
      pointAltitude="size"
      pointColor="color"
    />
  );
}

export default App;
```

After a few seconds you should see the basic globe demo appear in your browser. Love that globe.

## Building

This is done simply by

```bash
bash start.sh yarn build
```

## Cleanup

The container that we use is temporary, and gets cleaned up after you exit out. But we still have the docker image and docker volume floating around -- `node_modules` is always out to get you! -- so we'll need to clean that up after. You can delete this with:

```bash
docker image rm testapp
docker volume rm testapp_nodemodules
```

The next time you run `start.sh` this will all get recreated.
