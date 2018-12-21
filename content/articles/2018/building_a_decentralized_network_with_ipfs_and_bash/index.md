---
title: "Effigy: Building a decentralized social network with IPFS and bash"
subtitle: "and also in javascript"
date: 2018-12-01
draft: true
tags:
  - p2p
  - effigy
  - IPFS
  - scuttlebutt
---

I've been really interested in distributed software, especially scuttlebutt.  I find the technology with scuttlebutt -- node and electron -- to be a big pain to use, in an aestetic sense.  So lets see if we can adapt the same concepts to be able to run these networks in the different compute physical devices that I have on hand in an easy way.

- ChromeOS as a bash script inside of a linux container
- RaspberryPI as a bash script
- Chrome browser as a progress or desktop web app
- Android phone as a progressive webapp.

There are a lot of great things about scuttlebutt and it's community, the whole off grid/offline first concept is pretty amazing.  But lets see what we can do with basic linux tools and expand from there.

## Architecture

The basic idea is that there's going to be a distributed read only database for every device that you have.  We are going to keep track of the people that we follow using an IPNS name, which is a temporary mapping to different, yet permanent IPFS content keys.  So we will want a local cache of the last key we've seen from someone, and it might be useful to share those keys with anyone who is following us so that once the information gets on the network.

Each of the "databases" published on the otherwise of the IPNS name looks like:

- graph/QmcvfVxPeYmGk8TFd8fks6XM7NqeB6QZaKfpjvkMsZuafZ/latest
- graph/QmcvfVxPeYmGk8TFd8fks6XM7NqeB6QZaKfpjvkMsZuafZ/public.pem
- public.pem
- effigy.sh
- logs/1234.json
- logs/1235.json
- sigs/1234.sig
- sigs/1235.sig

The graph directory `graph` contains IPNS keys that this device is following, with the latest IPFS key that is known, public key.

`public.pem` is the public key to verify all the content on the logs.  Entries named with the unix time of the device that created the entries,

## Flow

The first time we follow a new User

tbd

## Identity is a private_key generated for the user

We are going to produce and distribute stream of messages that come from a specific device of the user.  They will probably have more than one device, especially over time, when we consider that people get new laptops and new computers when theirs break.  Identity will then be a concept built up top of these different keys.  Lets get generate the keys if needed and write a few functions that make sure we can sign messages and verify that they were correctly signed by the correct key.

The `private.pem` key will live and die on the device.  `public.pem` will be spread far and wide to the people looking at your content.  Perhaps there will be a mechanism to claim multiple public keys over time so that even if you lose the associated private key there's some degree of social continuity.


```bash
#!/bin/bash

if [ -z "$TMPDIR" ]; then
  TMPDIR=/tmp
fi
WORKDIR=~/.effigy/work
WORKDIR=work


# OPEN SSL VARIABLES
PRIVATEDIR=~/.effigy
PASSWORD=password

## OPENSSL Functions
validate_content_key() {
  if [ ! -f $PRIVATEDIR/private.pem ]; then
    echo Unable to find private key.

    if command -v openssl; then
      mkdir -p $PRIVATEDIR
      chmod 700 $PRIVATEDIR
      (
        cd $PRIVATEDIR
        openssl genrsa -aes128 -passout pass:$PASSWORD -out private.pem 4096 || return 1
        openssl rsa -in private.pem -passin pass:$PASSWORD -pubout -out public.pem || return 1
      )
    else
      echo Unable to find the openssl command
    fi
  fi
}

sign_message() {
  privatekey=$1
  infile=$2
  sigfile=$3

	openssl dgst -sha256 -passin pass:$PASSWORD \
		-sign $privatekey \
		$infile | \
	openssl base64 -out $sigfile

  return $?
}

verify_signature() {
  publickey=$1
  infile=$2
  sigfile=$3

  # TODO tempfiles
  # TODO tempfile name
  # TODO stdout ?
	openssl base64 -d -in $sigfile > /tmp/$sigfile
	openssl dgst -sha256 -verify $publickey -signature /tmp/$sigfile $infile > /dev/null

  rm -f /tmp/$sigfile

  return $?
}

##  TEST SSL
validate_content_key


if ! sign_message $PRIVATEDIR/private.pem newfeed.sh /tmp/newfeed.sig; then
  echo Unable to create signature
  exit 1
fi

if ! verify_signature $PRIVATEDIR/public.pem newfeed.sh /tmp/newfeed.sig ; then
  echo Unable to validate signatures
  exit 1
fi

```

We then try and sign a message with our private key, and then use the public key to validate that the signature is valid.

## Getting running with ipfs

Now we need to setup the ipfs instance locally if its not installed, generate a key that we'll use for IPNS to push our changes, and then make sure that the server is up an running.  This goes on the bottom of the existing file.

```bash

## IPFS variabes

DOTIPFS=$HOME/.ipfs
KEYNAME=effigy


validate_ipfs_install() {
  if ! command -v ipfs > /dev/null; then
   echo ipfs command not found.

   if ! install_ipfs_binary; then
     return 1
   fi
  fi

  if [ ! -d $DOTIPFS ]; then
    echo .ipfs not found, running init

    ipfs init
  fi

  return 0
}

# attempts to download the correct binary for host operation system and platform
install_ipfs_binary() {
  arch=`uname -m`
  if [[ "$arch" == "x86_64" ]]; then
    arch='amd64'
  fi

  platform=`uname -s | tr '[A-Z]' '[a-z]'`

  pattern="href=\"go-ipfs.*$platform-$arch"
  echo Looking for $arch for $platform

  dist_html=$TMPDIR/dist.ipfs.io.html

  if [ ! -r $dist_html ]; then
    curl http://dist.ipfs.io > $dist_html
  fi

  url=`grep $pattern $dist_html | sed 's/.*\"\(.*\)\".*/\1/'`

  download=$TMPDIR/goipfs.tgz

  if [ ! -r $download ]; then
    echo Downloading...
    curl http://dist.ipfs.io/$url > $download
  else
    echo Using cached version
  fi

  (
  cd $TMPDIR
  tar xzvf $download
  cd go-ipfs

  echo Enter root password to install ipfs in /usr/local/bin

  sudo ./install.sh
  )

  rm $download
  rm -rf $TMPDIR/go-ipfs

  if command -v ipfs; then
    return 0
  else
    return 1
  fi
}

validate_effigy_key() {
  if [ ! -f $DOTIPFS/keystore/$KEYNAME ]; then
    echo Generating key for $KEYNAME

    feed=$(ipfs key gen --type=rsa --size=2048 $KEYNAME)
    echo Your key is $feed
    mkdir -p $WORKDIR
    mkdir $WORKDIR/$feed
    touch $WORKDIR/$feed/tracking
  fi
}

validate_ipfs_daemon() {
  if ! ps -ef | grep -v grep | grep "ipfs daemon" > /dev/null; then
    echo IPFS Daemon is not running
    start_daemon
  fi
}

start_daemon() {
  echo Starting ipfs daemon
  ipfs daemon --enable-pubsub-experiment --enable-namesys-pubsub &
}

validate_ipfs() {
  validate_ipfs_install || return 1
  validate_effigy_key || return 1
  validate_ipfs_daemon || return 1
  return 0
}

effigy_ipns_name() {
  ipfs key list -l | awk '/effigy/ {print $1}'
}

if ! validate_ipfs; then
  echo Unable to validate the ipfs install
  exit 1
fi
```

## Messages

Lets start tieing things together to be able to create a message.

```bash
# Messages

new_message() {
  mkdir -p $WORKDIR
  cp $PRIVATEDIR/public.pem $WORKDIR

  messageid=$(date +"%s")
  messagefile=$WORKDIR/$messageid.json
  signaturefile=$WORKDIR/$messageid.sig

  read MESSAGE
  echo { \"message\": \"$MESSAGE\", \"messageid\": $messageid, \"parent\": \"$(effigy_ipns_name)\" } | jq . > $messagefile

  sign_message $PRIVATEDIR/private.pem $messagefile $signaturefile
}

new_message
```

First we create the work directory and copy the public key into it.  Then we create a filename with the current unix date, read the message in, and write a very simple json file out.  We include a link to the IPNS name that we are publishing under, so that you can look up the source information if this message is referenced from somewhere else.  We then sign that message using the openssl functions that we defined above.

## Feed
