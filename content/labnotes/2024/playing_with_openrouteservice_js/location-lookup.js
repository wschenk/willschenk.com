import Openrouteservice from 'openrouteservice-js'
import apiKey from './apiKey.js'

// So we don't do a zillion searches
export function debounce(func, timeout = 500){
    let timer;
    return (...args) => {
        clearTimeout(timer);
        timer = setTimeout(() => { func.apply(this, args); }, timeout);
    };
}

const Geocode = new Openrouteservice.Geocode({
    api_key: apiKey
})

class LocationLookup extends HTMLElement {
    connectedCallback() {
        const text = this.getAttribute( 'prompt' ) || "Location Search";
        
        this.insertAdjacentHTML( 'beforeend', `
          <p>${text}</p>
          <form>
            <input
              type="text"
              id="location"
              placeholder="Location"
              class="border-2 rounded"/>
          </form>
          <ul>
          </ul>
`);
        this.location = this.querySelector( 'input' );

        const processChange = debounce(() => this.lookupLocation());    
        this.location.addEventListener( 'keyup', processChange );

        this.results = this.querySelector( 'ul' );
        this.results.addEventListener( "click", this.fireEvent );
    }

    async lookupLocation() {
        this.results.innerHTML = `<li>Loading up ${location.value}</li>`
      
        const json = await Geocode.geocode( {text: this.location.value} )

        this.results.innerHTML = ""
        for( let i = 0; i < json.features.length; i++ ) {
            const feature = json.features[i];
            this.results.innerHTML +=
                `<li><a
                       data-lat="${feature.geometry.coordinates[1]}"
                       data-lon="${feature.geometry.coordinates[0]}"
                       class="text-blue-700">${feature.properties.label}</a></li>`
        }
    }

    fireEvent( event ) {
        let location = event.target.dataset;
        const myevent = new CustomEvent("location", {
            bubbles: true,
            detail: location
        });
        this.dispatchEvent( myevent );
    }
}

customElements.define( 'location-lookup', LocationLookup )
