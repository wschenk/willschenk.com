async function serverDown() {
    try {
        return fetch( "http://localhost:1234" )
            .catch( (e) => {
                console.log( "Promise error" );
                console.log(e);
            })
    } catch( e ) {
        console.log( "Exception error" );
        console.log( e );
    }
}

console.log( "Starting" );
await serverDown();
console.log( "Done" )
