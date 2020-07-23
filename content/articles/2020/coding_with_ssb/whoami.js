const ssb = require('ssb-client');

ssb( (err,sbot) => {
    if( err ) {
        console.log( err );
        process.exit(1);
    }

    console.log( sbot.id );
    process.exit(0);
})
