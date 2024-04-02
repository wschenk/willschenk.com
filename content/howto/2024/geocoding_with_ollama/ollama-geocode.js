import ollama from 'ollama/browser'
import schema_message from './schema_message.js'
import MapView from './map-view.js';

class OllamaGeocode extends HTMLElement {
    // This is the main method, which uses the same schema_message
    // as above
    async doLookup() {
        const output = await ollama.chat( schema_message( this.state.city ) )
        const response = output //JSON.parse( output )
        this.state = {
            loading: false,
            response: response,
            content: JSON.parse( response.message.content )
        }

        this.render();
    }

    static get observedAttributes() { return ["city"] };
    
    attributeChangedCallback( name ) {
        console.log( "Callback", name );
        this.lookup( this.getAttribute( "city" ) );
    }

    lookup( city ) {
        console.log( "Doing lookup for", city );
        this.state.loading = true;
        this.state.message = `Performing lookup for ${city}`
        this.state.city = city;

        this.doLookup()

        this.render();
    }

           
    connectedCallback() {
        this.state = {
            message: "Waiting for input"
        }
        this.render()
    }

    // Here I'm manually building the HTML from
    // the state object, but it's smart enough to use
    // WebComponents so it's not that bad.
    render() {
        let h = ""
        
        if( this.state.message ) {
            h += `<sl-alert open>${this.state.message}</sl-alert>`
        }

        if( this.state.loading ) {
            h += `<sl-progress-bar indeterminate py-2></sl-progress-bar>`
        }

        if( this.state.response ) {
            let r = this.state.response;
            h += `<sl-alert open>
  ${r.model} gave this answer in
  <sl-format-number value=${r.total_duration/ 1_000_000_000} maximumSignificantDigits="3"></sl-format-number>
  seconds</sl-alert>`
        }

        if( this.state.content ) {
            let c = this.state.content;
            h += `<sl-breadcrumb>
  <sl-breadcrumb-item>${c.country}</sl-breadcrumb-item>
  <sl-breadcrumb-item>${c.state}</sl-breadcrumb-item>
  <sl-breadcrumb-item>${c.city}</sl-breadcrumb-item>
</sl-breadcrumb>

<p>Population: ${c.population}</p>
<p>${c.description}</p>

<map-view latlon="${c.lat},${c.lon}" style="height: 200px"></map-view>`
        }
        this.innerHTML = h;
    }
}

customElements.define( 'ollama-geocode', OllamaGeocode )
