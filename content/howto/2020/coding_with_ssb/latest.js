const ssb = require('ssb-client')
const timeago = require('timeago.js')

ssb( (err, sbot) => {
    if( err ) {
        console.log( err );
        process.exit(1);
    }

    const id = process.argv[2] || sbot.id
    console.log( `Getting latest entry for ${id}` )
    sbot.getLatest( id, (err,latest) => {
        if( err ) {
            console.log( err );
            process.exit(1);
        }
        console.log( latest );

        console.log( "Posted", timeago.format( new Date( latest.value.timestamp ) ) )
        process.exit(0);
    });
} )
