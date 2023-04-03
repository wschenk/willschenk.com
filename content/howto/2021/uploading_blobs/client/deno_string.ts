fetch( 'http://localhost:8080/put',
       {method: 'POST', body: 'This is my string'} )
    .then( (res) => res.text() )
    .then( (res) => console.log( res ) )
