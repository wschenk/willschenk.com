const ssb = require('ssb-client')
const pull = require('pull-stream')
const delay = require('delay')

const onSsb = (err, sbot) => {
    if( err ) {
        console.log( err );
        process.exit(1);
    }

    const identity = process.argv[2] || sbot.id

    console.log( `Getting profile images for ${identity}` )

    pull(
        sbot.links( {
            source: identity,
            dest: identity,
            rel: 'about',
            values: true
        }),
        pull.collect( onProfile(sbot) )
    )
}

const onProfile = (sbot) => (err, data) => {
    if( err ) {
        console.log( err );
        process.exit(1)
    }

    //var pull = require('pull-stream')
    //pull(
    //sbot.blobs.get(hash),
    //pull.collect(function (err, values) {
    // eg values.join('') == 'hello, world'
    //    })
    //)

    data
        .filter( about => about.value.content.image )
        .map( about => {
            const i = about.value.content.image;

            console.log( about.value.content.image );
            console.log( `Getting ${i.link}` );
            pull(
                sbot.blobs.get( i.link ),
                pull.collect( (err, data) => {
                    console.log( "Something?" );
                    console.log( err, data )
                }))
        })

    delay( 2000 ).then( () => process.exit(0) );
}

ssb( onSsb );
