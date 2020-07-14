---
title: Playing with cabal
subtitle: serverless code
tags:
  - p2p
  - node
  - cabal
remote: git@github.com:wschenk/cabal-experiments.git
repository: https://github.com/wschenk/cabal-experiments
date: "2019-04-23"
---

Cabal is a "experimental p2p community chat platform".  It's fully distributed over the [dat protocol](https://www.datprotocol.com/).  When you create a new chat area -- something like a Slack -- it allows anyone with the same key to post and view messages everywhere.  When you post a message, everyone gets it and shares it with everyone else, so even when your computer drops off there will still be a coherent view of the data.

/Updated 2020-07-14 for cabal-core 5.0/

## Local computer

You'll need at least two computers to really play with this.  But we can simulate the situation by running a docker container as well as a local instance of the cabal cli client.  We can then move that container (or at least the image) around to run it on a remote server if you don't want to mess with anything on the remote machine.  Lets first install it locally:

## On the terminal

If you just want to play around you can use `npx`, or install the cabal cli command using `npm install --global cabal`.  Either way, lets start up a new cabal chat.

```bash
npx cabal --new
```

Take a note of the cabal URI on the top right of the screen, we'll use that to connect with later.  Keep this window running and start up a new Terminal window.  Type in a test message such as "this is the first message" (or something less banal).  There are also slash commands available, `/help` will list them.  Change your display nickname with `/n local node` or something like that.

## Dockerizing

This is optional, you can just repeat the same steps on the remote node if you want.  I have this in a Docker container because I don't want to install node on my remote server, but if you have you computers and aren't anal about it its super simple to just install node and just run `npx cabal` on them also.

I like keeping things in Docker containers when playing around so that no unnecessary packages get installed on the machine and the data files are nicely cleaned up when I exit out.

This is a very basic node-based Dockerfile.  Cabal/Dat uses port `3282` to communicate, so we'll need to expose and open that.

[`Dockerfile`](Dockerfile)
{{< highlight "Dockerfile" >}}
{{% raw "Dockerfile" %}}
{{< /highlight >}}

Then build like so:

```bash
docker build . -t $USER/cabal
```

Create a simple script to make it easier to run in the future.  (Note I'm using `--network host` here because I was having trouble getting dat to punch through the Docker networking/NAT stuff.)

{{< highlight "bash" >}}
{{% raw "run.sh" %}}
{{< /highlight >}}

This script is for running one off commands, and it throws away the container each time so you'll lose the history.  If you really wanted to use this for something more than an experiment you should mount the internal `/root/.cabal` directory to a docker volume and/or not make it a temporary container.

Push the container to the repository using `docker push $USER/cabal`, and then move everything to a different computer.  Then run:

```bash
bash run.sh cabal://f6e83732c84fe310515fe7162376e5420c6f4a27d015094ccc22be658b62d3c8
```

If all goes well, after a few moments of network dancing you should be able to see the original message you sent.  Type away to see if things show up!

## Using `cabal-core` in code

That's super cool already, but lets go a little deep and start writing our own handlers to see what we can do with `cabal` programmaticly.  `cabal` isn't secure at all so we probably shouldn't use this for anything real, but its a nifty way to communicate and discover without having to setup very much at all!

```bash
npm init
npm i add cabal-core tmp delay
```

## Printing out everything that is in a swarm

1. First we create a `Cabal` instance with a storage directory and our key, using `cabel = Cabal(directoy,key)`
2. Then we connect it to the swarm to start replicating using `swarm(cabal)`
3. Once the local database has been loaded and processed, the `cabal.ready()` callback is called.  Events received after this are new.
4. We can watch peers join and disconnect using `cabal.on('peer-added')` and `cabal.on('peer-dropped')`
5. We can listen for new messages and topics using `cabal.messages.events.on('message')`
6. (We can also listen for messages just in a specific channel, not shown below)
7. We can listen for new channels `cabal.channels.events.on( 'add' )`
8. We can get a current channel list using `cabal.channels.get()`
9. We can listen for new nicknames using `cabal.users.events.on('update')`

Here's it in code form [`dump.js`](read.js):

{{< highlight "js" >}}
{{% raw "dump.js" %}}
{{< /highlight >}}

Now lets restart everything and see what happens!  When I start up `node dump.js key` on my machine, and then start up the `cabal-cli` client on the other machine and:

1. Type `hello everyone`
2. Type `/n Will` - change my nickname to Will
3. Type `This is my new name`
4. Type `/join new channel`
5. Type `lets talk about this instead`
6. Type `/topic Serious Stuff`
7. Type `/quit`

{{< highlight "bash" >}}
{{% raw "dump.out" %}}
{{< /highlight >}}

## Publishing a message

Publishing a message involves appending to your local feed and then synchronizing to the swarm.  Which means waiting for the swarm to connect, a peer to join, and then them downloading everything.  We don't really know how long it will take, and there's no real way to guarantee that it will show up.  But we want a CLI tool that will eventually return, and not sit around forever.  Here's the logic we'll use:

1. Initialize cabal and try to connect to the swarm
2. Post the message to the local store
3. Once we get a peer connect message, wait 2 seconds for syncing and return success
4. If we don't get a peer connect in 10 seconds, then exit with error code.

Of course, if you just want to publish a message and serve the swarm forever, we could just take out the timeouts.

[`publish.js`](publish.js):

{{< highlight "js" >}}
{{% raw "publish.js" %}}
{{< /highlight >}}

Then try running this code with a running cabal node on the remote host and without one.  You'll notice that the messages that were stored locally will eventually be delivered once a connection is made.

## Next steps

One thing that I'm interested in playing around with is using this as a message bus of sorts, to each have remote devices report in on what they are doing (raspberry pi's for example) or be able to script it, to have a bot listening that takes commands in and run actions on the server and then pushes the results out (to IPFS for example.)  But that's for another post.

---

References

1. https://cabal-club.github.io/
1. https://www.datprotocol.com/
1. https://github.com/cabal-club
1. https://github.com/cabal-club/cabal-core
1. https://github.com/datproject/dat/issues/841
1. https://github.com/datproject/dat/issues/858
