---
title: "Image Manipulation in Firebase"
subtitle: "its all javascript"
date: 2018-11-23
tags:
  - firebase
  - howwo
  - javascript
  - images
---

We can manipulate images using JavaScript directly, which can be run both on the server or browser environment.  Lets take a look at how we'd do this using create-react-app and firebase.  We will deploy a function on firebase that will download the user's avatar, manipulate the image and overlay it with a mask, and then spit out an image.

## Project Setup
First make sure that you have `nvm` installed.  We'll need a different version of node for `create-react-app` then we will for firebase functions.

```bash
$ npx create-react-app honey
$ cd honey
$ firebase login
$ firebase init
```

- Select functions, hosting
- Select your proejct if you've already created it
- JavaScript
- Yes to ESLint
- No to install dependancies
- `build` instead of `public` directory
- Yes to single page app

Now we will create our functions to generate the image.  Firebase only supports node 6, so we'll need to set that up.  If you don't already have node 6 installed, make sure you do that with `nvm install 6`.

```bash
$ cd functions
$ nvm use 6
$ npm install firebase-functions@latest firebase-admin@latest express jimp tempfile node-fetch blueimp-md5 --save
```

Since that's installing another `node_modules` directory inside of the `functions` folder, be sure to add that to `.gitignore` in the project root:

```bash
echo /functions/node_modules >> ../.gitignore
```

## Write and test our function

We are going to combine an avatar.jpg like this:

<img src="avatar.jpg"/>

with a mask image like this:

<div><span style="background:#aaa;height:480px;width:480px;display:inline-block"><img src="results_mask.png"/></span></div>

to make an image like this:

<img src="output.png"/>

So right click and save the two images `avatar.jpg` and `results_mask.png` into your `functions` directory.  Then create the `functions/applyMask.js`:

```js
const Jimp = require( 'jimp' )
const Tempfile = require( 'tempfile' )

var maskFile = __dirname+"/results_mask.png";

var font = Jimp.loadFont( Jimp.FONT_SANS_32_WHITE );

function applyMask( text, avatarFile ) {
  return Jimp.read( maskFile ).then( (mask) => {
    return Jimp.read( avatarFile ).then( (image) => {
      return Jimp.loadFont( Jimp.FONT_SANS_32_WHITE ).then( (font) => {
        var original = image.clone()
        original.resize( 215, 215 ) // Resize clean avatar to fit in the circle

        image.resize( 480, 480 ) // Resize avatar to fill larger square
        image.blur(5) // Blur it
        image.color( [{ apply: 'darken', params: [60]}] ) // Darken the blurred colors

        // Center point is 238, 275
        // Write the clean avatar into the "center" of the circle
        image.composite( original, 238 - (original.bitmap.width/2), 275 - (original.bitmap.height/2), [Jimp.BLEND_DESTINATION_OVER, 1, 1] )
        // Write the mask on top
        image.composite( mask, 0, 0, [Jimp.BLEND_DESTINATION_OVER, 1, 1])

        // Write the text onto the image fitting it into the box
        // top left 156, 400 and 175 pixel across and 30 down
        image.print( font, 156, 400, {
          text: text,
          alignmentX: Jimp.HORIZONTAL_ALIGN_CENTER,
          alignmentY: Jimp.VERTICAL_ALIGN_MIDDLE
        }, 175, 30);

        var outputFile = Tempfile( ".png" );
        return image.writeAsync( outputFile ).then( () => { return outputFile} );
      } )
    } )
  } )
}

if( process.argv.slice(-1)[0] === '--test' ) {
  applyMask( "Will S", __dirname+"/avatar.jpg" )
    .then( console.log )
    .catch( console.log )
}

exports.applyMask = applyMask
```

We can test this function using `node avatarMask.js --test`, this will run the method directly so we can test out the results and print the output filename.  To open that file directly, you can do `open $(node avatarMask.js --test)`

This uses Jimp to:

1. Load in the mask using `Jimp.read`
2. Load in the avatar again using `Jimp.read`
3. Load in a font file that we will use for running
4. Clone the original image
5. Resize, blur and darken the avatar
6. Copy the original avatar onto the center point at 238, 275
7. Apply the mask on top
8. Write the user's name into the attached box
9. Create a tempfile to write the final image into
10. Resolve the promise at the end with the name of the tempfile.

The `Jimp` library is written in pure JavaScript so this code should be runnable in the browser also with some limited modifications.

## Write a Firebase function that returns the image

Lets first create a simple express function just to make sure that we can get the firebase functions to run inside of the emulator and attach to a https handler.

In `functions/index.js`:

```js
const functions = require('firebase-functions');
const express = require('express');

const webApp = express();

webApp.get( '/', (req, res) => {
  res.send( "Hello world")
})

exports.createImage = functions.https.onRequest( webApp )
```

The run

```bash
$ npm run serve
```

To run the function locally.  If it starts up correctly, you should see an `localhost` url that you can use, in my case `http://localhost:5000/honey-b6642/us-central1/createImage/`

Once that works, lets actually wire up our function to use our method (again this is `functions/index.js`)

```js
const functions = require('firebase-functions');
const express = require('express');
const { applyMask } = require('./merge');

const webApp = express();

webApp.get( '/', (req, res) => {
  console.log( "Generating image")
  applyMask( "Will S", __dirname+"/avatar.jpg" ).then( (tempfile) => {
    console.log( "Uploading file", tempfile)
    res.sendFile( tempfile )
  }).catch( (err) => {
    console.log( err )
    res.send( {error: err})
  } )
})

exports.createImage = functions.https.onRequest( webApp )
```

Again test with `node run serve`.  When you go the url, you should now see the image in the browser!

## Check to see that it works on firebase itself

`firebase deploy` will push the code to the firebase servers.  If you don't get any errors, you can see where the function is deployed either in the output logs, or by going to the [Firebase Console](https://console.firebase.google.com) and click on the `Functions` tab:

<img src="functions.png" class="img-fluid"/>

If you go to that url, https://us-central1-honey-b6642.cloudfunctions.net/createImage/ in the image above, (add a / at the end if you have a problem) you should see the image generated and loaded from the firebase function.

## Passing in the email and name and loading from gravatar

Lets change the code a bit to pass in the email and name, load the avatar from gravatar, and customize the image:

```js
const functions = require('firebase-functions');
const express = require('express');
const { applyMask } = require('./applyMask');
const Tempfile = require('tempfile')
const fetch = require('node-fetch')
const md5 = require('blueimp-md5')
const fs = require('fs')

const webApp = express();

function downloadGravatar( email ) {
  const hash = md5(email);
  return downloadUrl("https://www.gravatar.com/avatar/" + hash + ".jpg" )
}

function downloadUrl( url ) {
  console.log( "downloading " + url )
  return fetch( url ).then(res => {
    const outputFile = Tempfile( ".jpg" )
    console.log( "Streaming to ", outputFile )
    const dest = fs.createWriteStream(outputFile);
    res.body.pipe(dest);
    return outputFile;
  });
}

webApp.get( '/', (req, res) => {
  const email = req.query.email;
  let name = req.query.name;

  if( email === "" || email === undefined ) {
    return res.status(404).send( "Email not passed in")
  }

  if( name === "" || name === undefined ) {
    name = email
  }

  downloadGravatar( email ).then( (avatarFile) => {
    return applyMask( name, avatarFile )
  } ).then( (tempfile) => {
    console.log( "Uploading file " + tempfile )
    return res.sendFile( tempfile )
  }).catch( (err) => {
    console.log( err )
    res.send( {error: err})
  } )
})

exports.createImage = functions.https.onRequest( webApp )
```

Then start up your local firebase function with `npm run serve` and test it out with an email and name, for example: http://localhost:5000/honey-b6642/us-central1/createImage?email=wschenk@gmail.com&name=Will+S

Finally, deploy it all to firebase!

```
$ firebase deploy
```

The final code can be found https://github.com/wschenk/image_building_in_firebase

---

References

1. https://github.com/wschenk/image_building_in_firebase
2. https://github.com/oliver-moran/jimp
3. https://medium.com/@rossbulat/image-processing-in-nodejs-with-jimp-174f39336153
4. https://stackoverflow.com/questions/43117124/how-to-read-local-files-in-the-google-cloud-functions-emulator
5. https://en.gravatar.com/site/implement/images/
