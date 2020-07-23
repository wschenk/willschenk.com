const pull = require('pull-stream')

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

range( pull.values( [1,2,3,4] ), (e) => console.log( e ) )
