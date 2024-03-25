import Openrouteservice from 'openrouteservice-js'
import apiKey from './apiKey.js'

const Directions = new Openrouteservice.Directions({
    api_key: apiKey
});

export default class FindRoute extends HTMLElement {
    static get observedAttributes() {
        return ["from-latlon", "to-latlon" ];
    }
    
    attributeChangedCallback( name ) {
        if( name == 'from-latlon' ) {
            this.fromlatlon = this.getAttribute( "from-latlon" );
        }
        
        if( name == 'to-latlon' ) {
            this.tolatlon = this.getAttribute( "to-latlon" );
        }
        
        if( this.tolatlon && this.fromlatlon ) {
            this.doLookup();
        }
        
        this.render();
    }
    
    parseComma( latlon ) {
        let pair = latlon.split( "," );
        pair[0] = parseFloat(pair[0]);
        pair[1] = parseFloat(pair[1]);
        // hmm
        let swap = pair[0]
        pair[0] = pair[1]
        pair[1] = swap
        return pair;
    }

    async doLookup() {
        if( this.started == true ) {
            // To nothing
        }

        this.started = true;
        
        let response = await Directions.calculate({
            coordinates: [
                this.parseComma(this.fromlatlon),
                this.parseComma(this.tolatlon)
            ],
            profile: 'driving-car',
            format: 'json',
            api_version: 'v2'
        });

        console.log( "Directions response", response );

        this.started = false;
        this.response = response;
        this.render();
    }

    render() {
        let status = `
  <p>${this.fromlatlon}</p>
  <p>${this.tolatlon}</p>
  <p>${this.started ? "Loading" : "Done"}</p>
`
        if( this.response ) {
            status += '<table>';
            status += '<tr><th>Instruction</th><th>Distance</th><th>Duration</th><th>Name</th></tr>';
            
            for( let segment of this.response.routes[0].segments[0].steps ) {
                status += `<tr>
<td>${segment.instruction}</td>
<td>${segment.distance}</td>
<td>${segment.duration}</td>
<td>${segment.name}</td>
</tr>`
            }
        }
              
        this.innerHTML = status
          
    }
}

customElements.define( 'find-route', FindRoute )
