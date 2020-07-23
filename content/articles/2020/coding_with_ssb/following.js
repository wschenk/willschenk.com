const ssb = require('ssb-client')
const pull = require('pull-stream')
const timeago = require('timeago.js')

const onSsb = (err, sbot) => {
    if( err ) {
        console.log( err );
        process.exit(1);
    }
    const identity = sbot.id;

    console.log( `Getting profile entries for ${identity}` )

    pull(
        sbot.links( {
            source: identity,
            rel: 'contact',
            values: true
        }),
        pull.collect( onLinks )
    )
}

const onLinks = (err, data) => {
    if( err ) {
        console.log( err );
        process.exit(1)
    }

    data.map( about => {
        console.log( about );
        console.log( "Post", timeago.format( new Date( about.value.timestamp ) ) );
    })

    process.exit(0);
}

ssb( onSsb );
