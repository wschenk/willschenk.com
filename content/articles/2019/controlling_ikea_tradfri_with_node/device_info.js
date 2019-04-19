const Conf = require('conf');
const NodeTradfriClient = require("node-tradfri-client");
const delay = require( 'delay' );
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

  console.log( "Device " + device.name + " found" )
  let info = {
    type: device.type,
    name: device.name,
    model: device.deviceInfo.modelNumber,
  }

  if( device.type == 1  || device.type == 4 ) { // Remote control or motion sensor
    let deviceInfo = device.deviceInfo[0]
    if( deviceInfo ) {
      info.battery = deviceInfo.battery
    }
  } else if( device.type == 2 ) { // Light
    // console.log( device )
    let light = device.lightList[0]
    if( light ) {
      info.onOff = light.onOff
      info.color = light.color
      info.dimmer = light.dimmer
      info.colorTemperature = light.colorTemperature
      info.spectrum = light.spectrum
      info.hue = light.hue
      info.saturation = light.saturation
    }
  } else if( device.type == 3 ) {
    let plug = device.plugList[0]
    if( plug ) {
      info.onOff = plug.onOff
    }
  } else {
    console.log( "Unprocessed device type " + device.type + " name " + device.name )
    // console.log( device )
  }
  conf.set( key, info)
}

function tradfri_deviceRemoved(instanceId) {
  console.log( "Device Removed", instanceId)
  devices[instanceId] = null;
  conf.delete( 'devices.' + instanceId )
}

function tradri_groupUpdated(group) {
  // console.log( "group", group )
  console.log( "Group " + group.name + " found" )

  let key = 'groups.' + group.instanceId
  conf.set( key, {
    name: group.name,
    onOff: group.onOff,
    dimmer: group.dimmer,
    deviceIds: group.deviceIDs
  })

}

function tradri_groupRemoved(groupId) {
  console.log( "group removed" )
  conf.delete( 'devices.' + groupId )
}

function tradri_sceneUpdated( groupId, scene ) {
  console.log( "scene " + groupId + " updated")
  // console.log( scene)
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


  // observe devices
  tradfri
      .on("device updated", tradfri_deviceUpdated)
      .on("device removed", tradfri_deviceRemoved)
      .observeDevices()

  tradfri
      .on("group updated", tradri_groupUpdated )
      .on("group removed", tradri_groupRemoved )
      .on("scene updated", tradri_sceneUpdated )
      .on("scene removed", tradri_sceneRemoved )
      .observeGroupsAndScenes()


  return tradfri
}


getConnection().then( async (tradfri) => {
  while( !(devices[65538] && devices[65538].lightList) ) {
    console.log( "Waiting for device info to load")
    await delay( 500 )
  }
  console.log( conf.get( 'devices' ) )


  console.log( "Toggling lights")

  devices[65538].lightList[0].toggle(true)
  devices[65539].lightList[0].toggle(true)

  await delay( 2000 )

  devices[65538].lightList[0].setColor('f5faf6')
  devices[65539].lightList[0].setBrightness(50)

  await delay( 750 )

  devices[65538].lightList[0].setBrightness(75)
  devices[65539].lightList[0].setBrightness(20)


  console.log( "Toggling lights")
  devices[65538].lightList[0].toggle()
  devices[65539].lightList[0].toggle()

  console.log( "Hanging out for 10 seconds")

  await delay( 10000 )
  console.log( "Shutting down" )
  await tradfri.destroy();
  process.exit()
})
