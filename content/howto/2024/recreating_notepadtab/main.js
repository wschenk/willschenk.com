// main.js
import '@unocss/reset/tailwind.css';
import './main.css';
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace/dist/components/copy-button/copy-button.js';
import '@shoelace-style/shoelace/dist/components/textarea/textarea.js';
import '@shoelace-style/shoelace/dist/components/qr-code/qr-code.js';
import pako from 'pako';

function deflateToBase64( inputData ) {
    const compressed = pako.deflate( inputData );
    const base64 = window.btoa( String.fromCharCode.apply(null, compressed ));

    return base64;
}

function inflateFromBase64( base64 ) {
    const reverseBase64 = atob(base64);

    const reverseBase64Array = new Uint8Array(reverseBase64.split("").map(function(c) {
        return c.charCodeAt(0); }));

    const inflatedRaw = pako.inflate( reverseBase64Array );
    const decompressed = String.fromCharCode.apply( null, inflatedRaw );

    return decompressed;
}

window.addEventListener("load", (event) => {
    const box = document.getElementById( "note-area" )
    const copy = document.getElementById( "copy" );
    copy.value = window.location;
    
    const hash = window.location.hash
    if( hash != '' ) {
        // Restore the value
        const smaller = inflateFromBase64( hash.substring( 1 ) )
        box.value = smaller;
    }

    qrcode.value = window.location.href
    qrcode.size = 256;
    
    box.addEventListener( "sl-input", (event) => {
        // Update the url on change
        const hash = deflateToBase64( box.value )
        window.location.hash = '#' + hash
        copy.value = window.location
        qrcode.value = window.location.href
    });
});
