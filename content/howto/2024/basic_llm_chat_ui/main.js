import '@unocss/reset/tailwind.css';
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace';
import './generate-response.js';
import './llm-selector.js';
import './ollama-models.js';
import './openai-models.js';
import './main.css';

// For icons
import { setBasePath } from '@shoelace-style/shoelace/dist/utilities/base-path.js';
setBasePath('./node_modules/@shoelace-style/shoelace/dist');


// Wiring up stuff
promptinput.addEventListener( "keypress", (e) => {
    if( e.keyCode == 13 ) {
        const response = document.createElement( "generate-response" );
        response.setAttribute( 'llm', selector.llm );
        response.setAttribute( 'api_key', selector.api_key );
        response.setAttribute( 'query', promptinput.value );
        response.setAttribute( 'model', selector.model );
        chat.appendChild( response );

        console.log(chat)
        
        promptinput.value = "";
    }
})
