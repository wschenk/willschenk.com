import FindRoute from './find-route.js';
import polyline from '@mapbox/polyline';
import 'leaflet/dist/leaflet.css';
import L from 'leaflet';

class RouteDisplay extends FindRoute {
    connectedCallback() {
        this.insertAdjacentHTML( 'beforeend', '<div id="map" style="height:100%"></div>' );

        this.map = L.map('map', {
            //center: [ this.lat, this.lon ],
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

    render() {
        if( !this.response || !this.map) {
            return
        }
        
        const geometry = this.response.routes[0].geometry
        const decodedGeometry = polyline.decode(geometry);

        if( this.map && this.polyline ) {
            this.map.removeLayer( this.polyline )
            this.polyline = undefined;
        }
        
        this.polyline = L.polyline(decodedGeometry, {color: 'red'}).addTo(this.map);

        // zoom the map to the polyline
        this.map.fitBounds(this.polyline.getBounds());

      }
  }

  customElements.define( 'route-display', RouteDisplay )
