---
title: Controlling ikea Tradfi from your computer
subtitle: IKEA is so fun and its everywhere
tags:
  - howto
  - zigbee
  - IKEA
date: "2019-04-11"
draft: true
---


1. Setup IKEA Trådfri Gateway and Lights as normal
2. Install the `coap-client` binary
3. Figure out what the IP address is of the gateway
4. Get the security key off the back of the gateway
5.

First, set up a switch, lightbulb, and a gateway.  The gateway needs to be plugged into the router which is a bit of a pain.

## Install the `coal-client` binary

Then download and setup `libcoap` using the following steps.  Our goal is to get the coap-client binary.

On OSX

```bash
brew install libtool automake
```

On Linux:

```
sudo apt-get install build-essential autoconf automake libtool
```

Then on both:

```
git clone --recursive https://github.com/obgm/libcoap.git
cd libcoap
git checkout dtls
git submodule update --init --recursive
./autogen.sh
./configure --disable-documentation --disable-shared
make
sudo make install
```

You'll need to enter in your password to install the binary.  Check to see if it's installed correctly:

```bash
$ which coap-client
/usr/local/bin/coap-client
```

## Get the IP address and security code

On the back of the gateway the gateway mac security code is

Figure out where the gateway is on the network.  You can use a scanner like fing, or the command line.  In our case the hostname looks like `gw-b8d7af2a8e6f.home`, so we can just do a quick scan of the `arp` table to see what the ip address is.

```
$ arp -a | grep ^gw
gw-b8d7af2a8e6f.home (192.168.1.8) at b8:d7:af:2a:8e:6f on en0 ifscope [ethernet]
```

$ export IKEAGATEWAY=gw-b8d7af2a8e6f.home
$ export IKEASECURITY=aF5LF36XLnBGefj9



mkdir ikea
npm init
npm install --save node-tradfri-client


```js
import { discoverGateway } from "node-tradfri-client";

discoverGateway().then( result => console.log(result) );
```

// later:
const result = await discoverGateway();



First we need to get a user

```
coap-client -m post -u "Client_identity" -k "${IKEASECURITY}" -e '{"9090":"IDENTITY"}' "coaps://${IKEAGATEWAY}:5684/15011/9063"
```




Getting the state of the first lights

```
coap-client -m get -u "Client_identity" -k "${IKEASECURITY}" "coaps://${IKEAGATEWAY}:5684/15001/65537"
```

Turning off the first light

```
coap-client -m put -u "Client_identity" -k "${IKEASECURITY}" -e '{ "3311": [{ "5850": 0 }] }' "coaps://${IKEAGATEWAY}:5684/15001/65537"
```

## Code up some stuff




const Conf = require('conf');
const NodeTradfriClient = require("node-tradfri-client");
const { discoverGateway, authenticate, TradfriClient, Accessory, AccessoryTypes } = NodeTradfriClient;
const conf = new Conf()
let devices = {}

console.log( "Conf file is", conf.path)

if( !conf.has( 'devices' ) ) {
  conf.set( 'devices', {} )
}

function tradfri_deviceUpdated( device ) {
  devices[device.instanceId] = device;
  let key = 'devices.' + device.instanceId;
  if( !conf.has( key ) ) {
    console.log( "New device found ", device )
    conf.set( key, {type: device.type, name: device.name, isProxy: device.isProxy, model: device.deviceInfo.modelNumber})
  }
}

function tradfri_deviceRemoved(instanceId) {
  console.log( "Device Removed", instanceId)
  devices[instanceId] = null;
  conf.delete( 'devices.' + instanceId )
}

function tradri_groupUpdated(group) {
  console.log( "group", group )
}

function tradri_groupRemoved(groupId) {
  console.log( "group removed" )
}

function tradri_sceneUpdated( scene ) {
  console.log( "scene", scene)
}

function tradri_sceneRemoved( sceneId ) {
  conso.log( sceneId )
}

async function getConnection() {
  if( !conf.has( 'gateway.host' ) ) {
    console.log( "Looking up IKEA Tradfri gateway on your network" )
    let gateway = await discoverGateway()
    conf.set( 'gateway', gateway )
  }

  const tradfri = new TradfriClient(conf.get( 'gateway.host' ))

  if( !conf.has( 'security.identity' ) || !conf.has('security.psk' ) ) {
    let securityCode = process.env.IKEASECURITY
    if( securityCode === "" || securityCode === undefined ) {
      console.log( "Please set the IKEASECURITY env variable to the code on the back of the gateway")
      process.exit(1)
    }

    console.log( "Getting identity from security code" )
    const {identity, psk} = await tradfri.authenticate(securityCode);

    conf.set( 'security', {identity,psk} )
  }

  console.log( "Connecting to gateway" )

  await tradfri.connect(conf.get( 'security.identity' ), conf.get( 'security.psk' ))

  // observe devices
  tradfri
      .on("device updated", tradfri_deviceUpdated)
      .on("device removed", tradfri_deviceRemoved)
      .on("group updated", tradri_groupUpdated )
      .on("group removed", tradri_groupRemoved )
      .on("scene updated", tradri_sceneUpdated )
      .on("scene removed", tradri_sceneRemoved )
      .observeDevices()

  return tradfri
}

getConnection().then( (tradfri) => {
  setTimeout( () => {
    console.log( "There are " + Object.keys(devices).length + " devices we know about" )
    console.log( conf.get( 'devices' ) )
    const light = devices[65538].lightList[0];
    setTimeout(() => {console.log( "Turning on");light.turnOn()}, 500);
    setTimeout(() => {console.log( "Turning off");light.turnOff()}, 2000);
    setTimeout(() => {
      console.log( "Closing connection")
      tradfri.destroy()
      console.log( "Quitting" )
      process.exit()
    }, 5000)
    // setTimeout(() => {console.log( "Turning on");light.turnOn()}, 3000);
    // setTimeout(() => {console.log( "Turning off");light.turnOff()}, 5000);
  }, 1000 );
})

/*



      rl.question('What is the security code on the back of the gateway? ', (answer) => {
      // TODO: Log the answer in a database
      console.log(`Thank you for your valuable feedback: ${answer}`);

      rl.close();
    });
  }
}

async function getGateway() {
  if( conf.has('gateway') ) {
    console.log( "Returning gateway" )
    return conf.get( 'gateway' )
  }

  console.log( "Looking for gateway" )
  let gateway = await discoverGateway();
  conf.set( 'gateway', host: gateway.host )
  return gateway.host
}

async function getClient() {
  let host = await getGateway()
  return new TradfriClient(host)
}

async function getAuthenticateClient() {
  if( conf.has( 'security.identity') && conf.has( 'security.psk' ) ) {
    return
  }
}

async function  getIntentity(securityCode) {
  console.log( "Looking up gateway" )
  let gateway = await discoverGateway()

  const tradfri = new TradfriClient(gateway.host)

  console.log( "Getting identity from security code" )
  const {identity, psk} = await tradfri.authenticate(securityCode);

  return { identity, psk }
}

let identity = 'tradfri_1555023935192'
let psk = 'yQezpPUdvZJjXGzN'

// getIntentity( process.env.IKEASECURITY ).then( () => console.log( "Done") )



const lightbulbs = {};
function tradfri_deviceUpdated( device ) {
  console.log( device )
    if (device.type === AccessoryTypes.lightbulb) {
        // remember it
        lightbulbs[device.instanceId] = device;
    }
}

function tradfri_deviceRemoved(instanceId) {
  console.log( "Device Removed", instanceId)
    // clean up
}


async function blinkBulbs( identity, psk ) {
  let gateway = await discoverGateway()

  const tradfri = new TradfriClient(gateway.host)

  await tradfri.connect(identity, psk)

  // observe devices
  tradfri
      .on("device updated", tradfri_deviceUpdated)
      .on("device removed", tradfri_deviceRemoved)
      .observeDevices()

}

// blinkBulbs( identity, psk )
/*
// later...
// at least after we have actually received the light object

const light = lightbulbs[65537].lightList[0];
// blink
setTimeout(() => light.toggle(), 0);
setTimeout(() => light.toggle(), 1000);
setTimeout(() => light.toggle(), 2000);
setTimeout(() => light.toggle(), 3000);
*/


/*
// even later...
// before shutting down the app
tradfri.destroy();
*/


/*
console.log( "Looking up gateway" )
NodeTradfriClient.discoverGateway().then( result => console.log( result ) )


// connect
const tradfri = new TradfriClient("gw-abcdef012345");
try {
    await tradfri.connect(identity, psk);
} catch (e) {
    // handle error - see below for details
}


try {
    const {identity, psk} = await tradfri.authenticate(securityCode);
    // store identity and psk
} catch (e) {
    // handle error
}*/





---

References:

1. https://learn.pimoroni.com/tutorial/sandyj/controlling-ikea-tradfri-lights-from-your-pi
2.
