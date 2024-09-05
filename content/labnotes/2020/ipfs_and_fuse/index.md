---
title: IPFS and Fuse
subtitle: the worlds data in your filesystem
date: "2020-06-05"
tags:
  - ipfs
  - fuse
  - howto
aliases:
  - "/articles/2020/ipfs_and_fuse"
---

[IPFS](https://ipfs.io/) is cool. It's a peer to peer distributed file system, or graph database, where you reference the data by its hash. This has the great property that the data is no longer tied to a particular computer or server. It can be spread around and you can get it from anybody without worrying if they've changed or alterted it in any way. If you know what you are looking for, you don't need to worry about who you get it from.

Lets see how we can use the Fuse interface, so that we can use our regular shell scripting toolbox to access and process data on the network.

## Installing fuse on Linux

```bash
sudo apt-get install fuse
```

Make sure you are in the `fuse` group:

```bash
sudo groupadd fuse
sudo usermod -a -G fuse $USER
```

Edit `/etc/fuse.conf` and enable `user_allow_other`.

## Installing Fuse on OSX

I've not tried it, but here's [a link to the OSXFuse project](https://osxfuse.github.io/).

## Setting up the mount points

```bash
# make the directories
sudo mkdir /ipfs
sudo mkdir /ipns

# chown them so ipfs can use them without root permissions
sudo chown $USER /ipfs
sudo chown $USER /ipns
```

## Install IPFS

[Download ipfs](https://dist.ipfs.io/#go-ipfs) for your platform and install. e.g.:

```bash
cd $(mktemp -d)
wget https://dist.ipfs.io/go-ipfs/v0.5.1/go-ipfs_v0.5.1_linux-amd64.tar.gz
tar xzvf go-ipfs_v0.5.1_linux-amd64.tar.gz
cd go-ipfs
sudo sh install.sh
```

The initialise ipfs and start ipfs

```bash
ipfs init
ipfs config --json Mounts.FuseAllowOther true
ipfs daemon --mount
```

## Find some content

We can look up the CID of some files

```bash
dig +noall +answer TXT _dnslink.docs.ipfs.io| sed 's/.*\/ipfs/\/ipfs/;s/"//'
```

Which at the time of this writing outputs `/ipfs/bafybeicksctt6uom4iltnel6hqmgwaphlazrpzq5wv37bpjnmzintdq7su`.

And then we can go into that directory and start up a webserver.

```bash
cd /ipfs/bafybeicksctt6uom4iltnel6hqmgwaphlazrpzq5wv37bpjnmzintdq7su
```

Lets start up a webserver and check it out:

```bash
python -m SimpleHTTPServer
```

And when you visit [localhost:8000](http://localhost:8000) you have a copy of that specific version of their documentation site served from your local filesystem.

## Thoughts

The FUSE gateway isn't super stable, so there are some potential issues with this. But its an interesting distrubtion mechanism where you could have ngnix serving files right from /ipfs or /ipns, and be able to push changed locally on your computer and then have the webservers automatically pick them up.

This is also an easy way to interact with large files using a basic script that doesn't need to interact with the ipfs api's directly.
