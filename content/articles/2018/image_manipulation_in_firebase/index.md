---
title: "Image Manipulation in Firebase"
subtitle: "its all javascript"
draft: true
date: 2018-11-16
tags:
  - firebase
  - howtwo
  - javascript
---

npx create-react-app honey
cd honey
yarn add firebase
firebase login
firebase init

database, store, functions, hosting

choose a project

build instead of public

cd functions
npm install firebase-functions@latest firebase-admin@latest express jimp tempfile --save


add /functions/node_modules to .gitignore


copy and avatar.jpg results_mask.png into functions

`functions/merge.js`:

```js
const Jimp = require( 'jimp' )
const Tempfile = require( 'tempfile' )

var maskFile = __dirname+"/results_mask.png";
var avatarFile = __dirname+"/avatar.jpg";

async function applyMask( text ) {
  var mask = await Jimp.read( maskFile )
  var image = await Jimp.read( avatarFile )

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

  var font = await Jimp.loadFont(Jimp.FONT_SANS_32_WHITE)

  // Write the text onto the image fitting it into the box
  // top left 156, 400 and 175 pixel across and 30 down
  image.print( font, 156, 400, {
    text: text,
    alignmentX: Jimp.HORIZONTAL_ALIGN_CENTER,
    alignmentY: Jimp.VERTICAL_ALIGN_MIDDLE
  }, 175, 30);

  var outputFile = Tempfile( ".png" )
  await image.write( outputFile )
  return outputFile;
}

applyMask( "Will S" ).then( file => console.log( file ))
```

This uses Jimp to load in the avatar, resize it, apply the mask and then add the users name into the attached box.

Run this with `node merge.js | xargs open` to test.  Adjust as necessary.  When you are finished, replace the last line with

```js
exports.applyMask = applyMask
```

Then in `index.js`:

```js
const functions = require('firebase-functions');
const express = require('express');
// const applyMask = require('./merge');

const webApp = express();

webApp.get( '/createImage', (req, res) => {
  res.send( "Hello world")
  // const ref = functions.storage().ref()
  //
  // applyMask( "Will S").then( (tempfile) => {
  //   const output = ref.child( 'images/output.png' )
  //   output.put( tempfile ).then( () => {
  //     res.send( "File uploaded!" )
  //   })
  // })
})

exports.createImage = functions.https.onRequest( webApp )
```

firebase serve --only functions

---

References

1. https://github.com/oliver-moran/jimp
2. https://medium.com/@rossbulat/image-processing-in-nodejs-with-jimp-174f39336153
3. https://stackoverflow.com/questions/43117124/how-to-read-local-files-in-the-google-cloud-functions-emulator
