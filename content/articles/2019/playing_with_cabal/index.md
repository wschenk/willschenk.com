---
title: Playing with cabal
subtitle: serverless code
tags:
  - p2p
  - node
  - cabal
draft: true
date: "2019-04-21"
---

## Local computer

You'll need at last two computers to really play with this.  But we can simulate the situation by running a docker container as well as a local instance of the cabal cli client.  We can then move that container (or at least the image) around to run it on a remote server if you don't want to mess with anything.  Lets first install it locally:

## On the terminal

```bash
npm install --global cabal
```

Then start it up

```bash
cabal --new
```

_add screenshot_ ?

Take a note of the cabal URI on the top right of the screen, we'll use that to connect with later.  Keep this window running and start up a new Terminal window.  Type in a test message such as "this is the first message" (or something less banal).  There are also slash commands available, `/help` will list them.  Change your display nickname with `/n local node` or something like that.

## Dockerizing

This is optional, you can just repeat the same steps on the remote node if you want.

I like keeping things in Docker containers when playing around, which means that if I install something on my remote server but don't want to keep it around everything cleans up nicely without having random things in my node global directory for example, and the data files are clearly cleaned up.

This is a very basic node based Dockerfile.  Cabal/Dat uses port `3282` to communicate, so we'll need to expose and open that.

{{% code file="articles/2019/playing_with_cabal/Dockerfile" language="Dockerfile" %}}

Then build like so:

```
docker build . -t $USER/cabal
```

Then run thusly, replacing the cabal link with the one copied from the other terminal window.

```bash
docker run -it --rm -p 3282:3282 $USER/cabal cabal cabal://f6e83732c84fe310515fe7162....
```

yarn add cabal-core


---

References

1. https://cabal-club.github.io/
1. https://github.com/cabal-club
1. https://github.com/datproject/dat/issues/841
1. https://github.com/datproject/dat/issues/858
