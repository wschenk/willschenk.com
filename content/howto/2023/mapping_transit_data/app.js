let stops = [];
let map = undefined;

document.addEventListener("DOMContentLoaded", () => {
    mapboxgl.accessToken =
        'pk.eyJ1Ijoid3NjaGVuayIsImEiOiJjazl2Nzd4NjcwOWU3M21xbzRzdjkwNDV0In0.dp3uaAniKU9DODuTwH7CwQ';

    map = new mapboxgl.Map({
        container: 'map',
        style: 'mapbox://styles/mapbox/outdoors-v12'
    });
    
    map.addControl(new mapboxgl.NavigationControl());

    loadStops();
});

function loadStops() {
    console.log( "loading stops.json" );

    return fetch( "stops.json" ).
        then( (data) => data.json() ).
        then( (json) => {
            stops = json;
            addPinsAndRecenter();
        })
}

function boundingBox() {
    let mnLL = [stops[0].stop_lon, stops[0].stop_lat];
    let mxLL = [stops[0].stop_lon, stops[0].stop_lat];

    for( let stop of stops ) {
        if( stop.stop_lon < mnLL[0] ) {
            mnLL[0] = stop.stop_lon;
        }

        if( stop.stop_lat < mnLL[1] ) {
            mnLL[1] = stop.stop_lat;
        }

        if( stop.stop_lon > mxLL[0] ) {
            mxLL[0] = stop.stop_lon;
        }

        if( stop.stop_lat > mxLL[1] ) {
            mxLL[1] = stop.stop_lat;
        }
    }

    return [mnLL, mxLL];
}
        

function addPinsAndRecenter() {
    console.log( "Recentering" );
    map.fitBounds( boundingBox() );

    console.log( "Adding pins" );
    for( let stop of stops ) {
        // Create a new marker.
        const marker = new mapboxgl.Marker()
              .setLngLat([stop.stop_lon, stop.stop_lat])
              .addTo(map);
    }
}

let shapes = {}

document.addEventListener("DOMContentLoaded", () => {
    loadShapes();
})

function loadShapes() {
    return fetch( "shapes.json" ).
        then( (data) => data.json() ).
        then( (json) => {
            // Add to hash
            for( let shape of json ) {
                shapes[shape.shape_id] ||= []
                shapes[shape.shape_id].push(shape)
            }

            console.log( "shapes", shapes );

            // Sort segments
            for( let shape_id in shapes ) {
                let s = shapes[shape_id]
                s.sort( (a,b) => { a.shape_pt_sequence - b.shape_pt_sequence } )
                addMapLine( s )
            }
        })
}

function addMapLine( shape ) {
    map.on('load', () => {
        let coordinates = []
        for( let point of shape ) {
            coordinates.push( [point.shape_pt_lon, point.shape_pt_lat] );
        }

        console.log( "coord", coordinates )
        let name = `shape_${shape[0].shape_id}`

        map.addSource(name, {
            'type': 'geojson',
            'data': {
                'type': 'FeatureCollection',
                'features': [
                    {
                        'type': 'Feature',
                        'properties': {
                            'color': '#F7455D' // red
                        },
                        'geometry': {
                            'type': 'LineString',
                            'coordinates': coordinates
                        }
                    },
                ]
            }
        });
        map.addLayer({
            'id': name,
            'type': 'line',
            'source': name,
            'paint': {
                'line-width': 3,
                'line-color': ['get', 'color']
            }
        });
    } );
    console.log("done" )
}
