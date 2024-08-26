import * as fs from 'fs';
import path from 'node:path';
import process from 'node:process';

export const dir = process.env.DATA_DIR ? process.env.DATA_DIR :
    path.normalize( path.join( process.cwd(), 'upload' ));

try {
    if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir);
    }
} catch (err) {
    console.error(err);
}

export function entries() {
    const all_files = fs.readdirSync(dir, {withFileTypes: true})
    const files = all_files.filter((dirent) => dirent.isFile()).map((f) => f.name);
    
    return files;
}

export function storeImage(image) {
    const count = entries().length
    const ext = path.extname(image.name);
    const name = `${dir}/${count}${ext}`
    
    console.log( "Storing file in ", name );
    
    image.mv( name )
}

export function realPath(name) {
    const basename = path.basename(name);
    return path.normalize(`${dir}/${basename}`)
}

console.log( entries() )
