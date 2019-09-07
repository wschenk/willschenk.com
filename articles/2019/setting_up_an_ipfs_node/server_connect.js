const IPFS = require( 'ipfs' );

(async() => {
    console.log( "Connecting to our server" );
    const node = await IPFS.create({
//	config: {
//	    Addresses: {
//		Swarm: [
//		    '/dns4/ssb.willschenk.com/tcp/443/wss/p2p-websocket-star'
//		]
//	    }
//	}
    });

    console.log( "Looking up our node info" );

    console.log( await node.id() );

    console.log( "Quitting" );
    process.exit(0);
})();


