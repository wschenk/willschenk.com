const fs = require('fs');
const lockfile = require('@yarnpkg/lockfile');
//import fs from 'fs';
//import * as lockfile from '@yarnpkg/lockfile';

let file = fs.readFileSync('/home/wschenk/gratitude/yarn.lock', 'utf8');
let json = lockfile.parse(file);

console.log(json);
