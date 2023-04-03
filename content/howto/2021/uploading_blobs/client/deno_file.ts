const form = new FormData()
const file = await Deno.readFile( '/home/wschenk/mobiledownloads/talk.pdf' )
const blob = new Blob( [file] )
form.append( 'file', blob,  'testfilename')

const options = {
    method: 'POST',
    body: form }

fetch( 'http://localhost:8080/put', {...options})
    .then( res => res.text() )
    .then( res => console.log( res ) );
