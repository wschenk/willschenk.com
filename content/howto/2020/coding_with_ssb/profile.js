const ssb = require('ssb-client')
const pull = require('pull-stream')
//const timeago = require('timeago.js')

const range = (source, closure ) => {
    console.log( "Hi range" )
    let done = false;
    while( !done ) {
        source( null, (end, value) => {
            if( end ) {
                done = true;
            } else {
                closure( value )
            }
        })
    }
}

const onSsb = (err, sbot) => {
    if( err ) {
        console.log( err );
        process.exit(1);
    }

    const identity = process.argv[2] || sbot.id

    console.log( `Getting profile entries for ${identity}` )

    pull(
        sbot.links( {
            source: identity,
            dest: identity,
            rel: 'about',
            values: true
        }),
        pull.collect( onProfile )
    )
}

const onProfile = (err,data) => {
    console.log(data);
    process.exit(0);
}

ssb( onSsb );
