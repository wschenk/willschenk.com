'use strict';

const fs = require('fs');

let rawdata = fs.readFileSync('package-lock.json');
let lockfile = JSON.parse(rawdata);

Object.keys(lockfile.dependencies).forEach( (elem,index) => {
    console.log( `| ${index+1} | ${elem} | ${lockfile.dependencies[elem].version} |` );
});
