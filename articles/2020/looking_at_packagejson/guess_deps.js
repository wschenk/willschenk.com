'use strict';

const fs = require('fs');

let rawdata = fs.readFileSync('package-lock.json');
let lockfile = JSON.parse(rawdata);

let deps = Object.keys(lockfile.dependencies);
let possible_mains = new Map();

deps.forEach( elem => {
    possible_mains.set( elem,  true )
} );

deps.forEach( elem => {
    let requires = Object.keys(lockfile.dependencies[elem].requires || {})
    requires.forEach( dep => {
        possible_mains.delete( dep );
    } )
} )

possible_mains.forEach( (value, key) => console.log( key ) );
