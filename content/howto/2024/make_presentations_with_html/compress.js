// compress.js
import pako from 'https://cdn.jsdelivr.net/npm/pako@2.1.0/+esm'

export function deflateToBase64( inputData ) {
    const compressed = pako.deflate( inputData );
    const base64 = window.btoa( String.fromCharCode.apply(null, compressed ));
    
    return base64;
}

export function inflateFromBase64( base64 ) {
    const reverseBase64 = atob(base64);

    const reverseBase64Array = new Uint8Array(reverseBase64.split("").map(function(c) {
        return c.charCodeAt(0); }));

    const inflatedRaw = pako.inflate( reverseBase64Array );
    const decompressed = String.fromCharCode.apply( null, inflatedRaw );

    return decompressed;
}
