const Cabal = require('cabal-core');
const swarm = require('cabal-core/swarm.js');
const tmp = require('tmp');

if( process.argv.length != 3 ) {
  console.log( "Usage" )
  console.log( "node dump.js cabalkey")
  process.exit(1)
}

// Strip out the awesome
const key =  process.argv[2].replace('cabal://', '').replace('cbl://', '').replace('dat://', '').replace(/\//g, '')
console.log( "Connecting to", key)

// For testing to keep things clean
const storage_dir = process.env.CABAL_DIR || tmp.dirSync().name;
console.log( "Storing our database in", storage_dir);

// Create a cabal instance
const cabal = Cabal( storage_dir, key, {maxFeeds:1000} )

console.log( "Connecting to swarm")
swarm(cabal);

// This is called after the local database is loaded
cabal.ready( () => {console.log( "Database is ready!")})

// Print out connect and disconnect messages
cabal.on('peer-added', (key) => console.log( "Peer added",  key ) );
cabal.on('peer-dropped', (key) => console.log( "Peer dropped", key) );

// Prints messages as they come in
cabal.messages.events.on('message', (message) => console.log("Got a message", message))

// Print user messages as they come in
cabal.users.events.on('update', (key, message) => console.log( "user message", message ))

// Print out channel list
cabal.channels.get( (err,channels) => console.log( "Channels", channels))
cabal.channels.events.on( 'add', (channel) => console.log( "new channel", channel))

// Process will hang waiting for events
