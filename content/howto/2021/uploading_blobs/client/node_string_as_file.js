const fetch = require( 'node-fetch' );
const FormData = require( 'form-data' );

function write_string_blob( url, string ) {
    const form = new FormData();
    form.append('file', string, {filename: 'test'} );

    const options = {
        method: 'POST',
        credentials: 'include',
        body: form
    };

    return fetch(url, { ...options })
        .then(res => {
            if (res.ok) return res.text();
            throw res;
        });
}

write_string_blob( 'http://localhost:8080/put', 'This is my string').
    then( (res) => console.log( res ) )
