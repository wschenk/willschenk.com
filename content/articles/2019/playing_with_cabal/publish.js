const Cabal = require('cabal-core');
const swarm = require('cabal-core/swarm.js');
const delay = require('delay');
const tmp = require('tmp');

if( process.argv.length != 5 ) {
  console.log( "Usage" )
  console.log( "node publish.js cabalkey channel message")
  process.exit(1)
}

// Strip out the awesome
const key =  process.argv[2].replace('cabal://', '').replace('cbl://', '').replace('dat://', '').replace(/\//g, '')
console.log( "Connecting to", key)
const channel = process.argv[3]
const message = process.argv[4]

// For testing to keep things clean
const storage_dir = process.env.CABAL_DIR || tmp.dirSync().name;
console.log( "Storing our database in", storage_dir);

// Create a cabal instance
const cabal = Cabal( storage_dir, key, {maxFeeds:1000} )

console.log( "Connecting to swarm")
swarm(cabal);

cabal.on('peer-added', (key) => {
  console.log( "Peer added",  key );
  delay( 2000 ).then( () => {
    console.log( "Quitting 2 seconds after peer connect" )
    process.exit(0)
  })
})

let messageStored = false
function publishCallback() {
  messageStored = true;
  console.log( "Message stored locally" )
}

// This is called after the local database is loaded
cabal.ready( () => {
  console.log( "Database is ready!")

  cabal.publish( {type: "chat/text", content: { text: message, channel}}, publishCallback );
})

delay( 10000 ).then( () => {
  if( messageStored ) {
    console.log( "10 second time before swarm connect, stored locally");
    process.exit(1)
  } else {
    console.log( "10 second timeout before message was stored");
    process.exit(1);
  }
})
