import 'leaflet/dist/leaflet.css';
import L from 'leaflet';

class MapView extends HTMLElement {
    static get observedAttributes() {
        return ["latlon"];
    }
    
    connectedCallback() {
        this.insertAdjacentHTML( 'beforeend', '<div id="map" style="height:100%"></div>' );

        this.setLatLon();
        
        this.map = L.map('map', {
            center: [ this.lat, this.lon ],
            zoom: 11,
            zoomControl: true
        });

        L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 19,
            attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
        }).addTo(this.map);

        L.control.scale({
            imperial: true,
            maxWidth: 300
        }).addTo(this.map);

        this.setMarker();
    }

    setLatLon() {
        let latlon = this.getAttribute( 'latlon' ) || '41.84371,-73.32928';

        let s = latlon.split( "," );
        this.lat = s[0];
        this.lon = s[1];
        this.setMarker()
    }

    attributeChangedCallback( name ) {
        if( name == 'latlon' ) {
            this.setLatLon();
        }
    }

    setMarker() {
        if( this.map ) {
            this.map.setView( [this.lat, this.lon] );
            if( this.marker ) {
                this.marker.remove();
            }

            this.marker = L.marker([this.lat, this.lon]).addTo(this.map);
        }
    }
}

customElements.define( 'map-view', MapView )
