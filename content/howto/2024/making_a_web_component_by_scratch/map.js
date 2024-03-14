import 'leaflet/dist/leaflet.css';
import L from 'leaflet';

class MapComponent extends HTMLElement {
    connectedCallback() {

        this.insertAdjacentHTML( "beforeend", "<div id='map'></div>" );

        let mapdiv = this.querySelector( "#map" );
        mapdiv.style.cssText = `
height: 400px;
width: 600%;
z-index:0;
max-width: 100%;
max-height: 100%;
`
        
        let lat = this.getAttribute( "lat" )
        let lon = this.getAttribute( "lon" )

        console.log( "lat", lat );
        console.log( "lon", lon );

        let map = L.map('map').setView([lat, lon], 13);

        L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 19,
            attribution: '&copy; OpenStreetMap'
        }).addTo(map);
    }
}

customElements.define( 'map-component', MapComponent )
