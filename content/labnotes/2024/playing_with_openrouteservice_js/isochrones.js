import Openrouteservice from 'openrouteservice-js'
import apiKey from './apiKey.js'
import 'leaflet/dist/leaflet.css';
import L from 'leaflet';

const Isochrones = new Openrouteservice.Isochrones({
    api_key: apiKey
})

class IsochroneMap extends HTMLElement {
    static get observedAttributes() {
        return ["latlon"];
    }

    attributeChangedCallback( name ) {
        if( name == 'latlon' ) {
            this.setLatLon();
        }
    }

    setMarker() {
        if( this.map && this.lat && this.lon ) {
            this.map.setView( [this.lat, this.lon] );
            if( this.marker ) {
                this.marker.remove();
            }
            
            this.marker = L.marker([this.lat, this.lon]).addTo(this.map);
        }
    }
    
    
    setLatLon() {
        let latlon = this.getAttribute( 'latlon' ) || '41.84371,-73.32928';
        
        let s = latlon.split( "," );
        this.lat = s[0];
        this.lon = s[1];
        this.setMarker()

        this.lookup();
    }

    lookup() {
        if( this.lat && this.lon ) {
            console.log( "Doing lookup" );
            Isochrones.calculate( {
                profile: 'driving-car',
                locations: [[this.lon, this.lat]],
                range: [50000],
                range_type: 'distance',
                area_units: 'm'
            } ).then( (json) => {
                console.log( json );
                this.response = json;

                let c = json.features[0].geometry.coordinates[0];
                this.latlngs = []
                for( let i = 0; i < c.length; i++ ) {
                    this.latlngs.push([c[i][1],c[i][0]]);
                }
                
                if( this.map && this.polygon ) {
                    this.map.removeLayer( this.polygon )
                    this.polygon = undefined;
                }

                this.polygon = L.polygon(this.latlngs, {color: 'red'}).addTo(this.map);
                this.map.fitBounds(this.polygon.getBounds());
            })
        }
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
    }
}
    
customElements.define( 'isochrone-map', IsochroneMap );
