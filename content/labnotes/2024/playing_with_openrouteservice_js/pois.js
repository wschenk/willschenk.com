import Openrouteservice from 'openrouteservice-js'
import apiKey from './apiKey.js'

const Pois = new Openrouteservice.Pois({
    api_key: apiKey
})

class PoisMap extends HTMLElement {
    static get observedAttributes() {
        return ["latlon"];
    }
    
    attributeChangedCallback( name ) {
        if( name == 'latlon' ) {
            this.setLatLon();
        }
    }

    connectedCallback() {
        this.setLatLon();
        }

    setLatLon() {
        let latlon = this.getAttribute( 'latlon' ) || '41.84371,-73.32928';
        
        let s = latlon.split( "," );
        this.lat = parseFloat(s[0]);
        this.lon = parseFloat(s[1]);
        
        this.lookup();
    }

    async lookup() {
        let box = [[this.lon, this.lat],
                   [this.lon - 0.05, this.lat - 0.05]]

        Pois.pois({
            geometry: {
                bbox:box,
                geojson:{
                    type:"Point",
                    coordinates:box[0],
                },
                buffer:250
            },
            timeout: 20000
        }).then( (json) => {
            console.log( json );
            this.response = json;

            this.render();
        })
    }

    render() {
        let stats = "<table><tr><th>Name</th><th>Category</th><th>Coors</th></tr>";

        if( this.response ) {
            for( let poi of this.response.features ) {
                console.log( poi );
                let name = ""
                if( poi.properties.osm_tags ) {
                    name = poi.properties.osm_tags.name;
                }
                stats += `<tr>
<td>${name}</td>
<td>${JSON.stringify(poi.properties.category_ids)}</td>
<td>${poi.geometry.coordinates}</td>
</tr>`;
            }
        }

        stats += "</table>";
        this.innerHTML = stats;
    }
}

customElements.define( 'pois-map', PoisMap );
