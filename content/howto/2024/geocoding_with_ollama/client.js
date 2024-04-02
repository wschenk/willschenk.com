import '@unocss/reset/tailwind.css';
import '@shoelace-style/shoelace/dist/themes/light.css';
import '@shoelace-style/shoelace';
import './main.css';

city.addEventListener( 'sl-change', (e) => {
    geocoder.setAttribute( "city", city.value );
} )
