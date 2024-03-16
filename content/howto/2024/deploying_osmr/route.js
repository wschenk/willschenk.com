var version = 'v5';
var osrmTextInstructions = require('osrm-text-instructions')(version);

fetch(
    "http://127.0.0.1:5000/route/v1/driving/-73.364755,41.838942;-73.409656,41.752237?steps=true" )
    .then( (response) => response.json() )
    .then( (response) => {
        response.routes[0].legs.forEach(function(leg) {
            leg.steps.forEach(function(step) {
                instruction = osrmTextInstructions.compile('en', step)
                console.log( instruction )
            });
        })
    })
