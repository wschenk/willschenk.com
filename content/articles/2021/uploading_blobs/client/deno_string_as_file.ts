const form = new FormData()
const blob = new Blob(['This is my string'])
form.append( 'file', blob,  'testfilename')

const options = {
    method: 'POST',
    body: form }

fetch( 'http://localhost:8080/put', {...options})
    .then( res => res.text() )
    .then( res => console.log( res ) );
