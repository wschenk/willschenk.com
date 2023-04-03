'use strict';

const NpmApi = require( 'npm-api' );
const fs = require('fs');

let rawdata = fs.readFileSync('package-lock.json');
let lockfile = JSON.parse(rawdata);

(async () => {    
    Object.keys(lockfile.dependencies).forEach( async (elem,index) => {
        const repo = new NpmApi().repo( elem );
        const pkg = await repo.package();

        const installed_version = lockfile.dependencies[elem].version;
        const current_version = pkg.version;

        console.log( `| ${elem} | ${installed_version} | ${current_version} | ${installed_version != current_version ? 'OUTDATED' : 'CURRENT'} |` );
    });
})()
