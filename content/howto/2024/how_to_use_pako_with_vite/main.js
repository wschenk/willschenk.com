import pako from 'pako';

window.addEventListener("load", (event) => {
    const source = document.getElementById( "source" );
    const encoding = document.getElementById( "encoding" );
    const dest = document.getElementById( "dest" );
    
    source.addEventListener( "keyup", (e) => {
        // Compress and encode
        const inputData = source.value;
        console.log( "inputData", inputData );

        const compressed = pako.deflate( inputData );
        console.log( "compressed", compressed );

        const base64 = window.btoa( String.fromCharCode.apply(null, compressed ));
        encoding.innerHTML = base64;
        console.log( "base64", base64 );

        // Stats
        rawsize.innerHTML = inputData.length;
        hashsize.innerHTML = base64.length;

        // Decompress and inflate back to the original string
        const reverseBase64 = atob(base64);

        // Converts the string back to an Uint8Array
        const reverseBase64Array = new Uint8Array(reverseBase64.split("").map(function(c) {
            return c.charCodeAt(0); }));
        console.log( "reverse", reverseBase64Array );

        const inflatedRaw = pako.inflate( reverseBase64Array );
        console.log( "inflatedRaw", inflatedRaw );


        // Convert back to string
        const decompressed = String.fromCharCode.apply( null, inflatedRaw );
        console.log( "decompressed", decompressed );
        dest.innerHTML = decompressed;
    } )
} )
