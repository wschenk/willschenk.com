import '@unocss/reset/tailwind.css';
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace/dist/shoelace.js';
import './generate-response.js';
import './main.css';

promptinput.addEventListener( "keypress", (e) => {
    if( e.keyCode == 13 ) {
        const response = document.createElement( "generate-response" );
        response.setAttribute( 'query', promptinput.value );
        chat.appendChild( response );

        console.log(chat)
        
        console.log( "submit" )
        promptinput.value = "";
    }
})
