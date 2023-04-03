const fetch = require( 'node-fetch' );
const FormData = require( 'form-data' );
const fs = require('fs');
const path = require('path')

function write_file_blob( url, filename ) {
    const form = new FormData();
    const buffer = fs.readFileSync(filename);

    form.append('file', buffer, {filename: path.basename( filename )} );

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

write_file_blob( 
    'http://localhost:8080/put',
    '/home/wschenk/mobiledownloads/talk.pdf'
).
    then( (res) => console.log( res ), (rej) => console.log( rej ) )
