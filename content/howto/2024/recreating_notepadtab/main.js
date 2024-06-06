// main.js
import '@unocss/reset/tailwind.css';
import './main.css';
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace/dist/shoelace.js';

window.addEventListener("load", (event) => {
    const box = document.getElementById( "note-area" )
    const copy = document.getElementById( "copy" );
    copy.value = window.location;
    
    const hash = window.location.hash
    if( hash != '' ) {
        // Restore the value
        const smaller = window.atob( hash.substring( 1 ) )
        box.value = smaller;
    }

    box.addEventListener( "sl-input", (event) => {
        // Update the url on change
        const hash = window.btoa( box.value )
        window.location.hash = '#' + hash
        copy.value = window.location
    });
});
