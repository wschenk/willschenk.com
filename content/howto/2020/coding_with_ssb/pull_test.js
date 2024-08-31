const pull = require('pull-stream')

/*
const range = ( source, closure ) => {
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
*/
function range(source) {
    return new Promise( (resolve, reject) => {
        let values = []
        pull( source,
              pull.drain( (e) => values.push(e), (err) => {
                  if( err ) {
                      reject(err);
                  } else {
                      resolve(values);
                  }
              }))
    })
}
      
(async () => {
    values = await range( pull.values([1,2,3,4]) )
    console.log( values );
})()

const ssb = require('ssb-client')

async function to_a( source ) {
    return new Promise( (resolve, reject) => {
        let values = [];
        pull( source,
              pull.drain( (v) => {
                  values.push(v)
              },
                          (err) => {
                              console.log( "Done" );
                              if( err ) {
                                  reject(err);
                              } else {
                                  resolve(values);
                              }
                          } )
            )
    })
}

ssb( async (err, sbot) =>  {
    if( err ) {
        console.log( err );
        process.exit(1);
    }

    const identity = sbot.id;

    contacts = sbot.links( {
        source: identity,
        rel: 'contact',
        values: true
    })

    console.log( "Looping over contacts" )
    v = await to_a( contacts )
    console.log( `Found ${v.length} entries` );

    process.exit(0);
} )
