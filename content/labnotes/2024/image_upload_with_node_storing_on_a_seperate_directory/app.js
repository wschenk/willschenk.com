import express from 'express'
import fileUpload from 'express-fileupload'
import { storeImage, realPath, entries } from './file_storage.js'
import cors from 'cors';

const app = express();
//if( import.meta.env.MODE == 'development' ) {
    app.use(cors())
//}

app.use(fileUpload());

const port = 3000;

app.use(express.static('dist/'));

app.get('/', (req, res) => {
    res.send('Hello World!');
});

app.get( '/images', (req, res) => {
    const list = entries().map( (elem) => `/images/${elem}` ) 
    const ret = {
        entries: list
    }

    res.json( ret )
})

app.get( '/images/:path', (req, res) => {
    res.sendFile( realPath(req.params.path) );
} )


app.post('/upload', (req, res) => {
    // Get the file that was set to our field named "image"
    const { image } = req.files;

    // If no image submitted, exit
    if (!image) return res.sendStatus(400);

    storeImage( image );
    // Move the uploaded image to our upload folder
    //image.mv( './upload/' + image.name);

    res.sendStatus(200);
});

app.listen(port, () => {
    console.log(`Example app listening on port ${port}`);
});
