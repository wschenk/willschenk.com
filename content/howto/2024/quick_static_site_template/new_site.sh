#!/bin/bash

cd $(mktemp -d)
echo We are now working in $(pwd)

echo Installing vite and unocss
npm i vite unocss

echo Setting up .gitignore
echo node_modules/ >> .gitignore

echo Adding scripts to package.json
mv package.json _package.json
cat > scripts.json <<SCRIPTEOF
{
  "scripts": {
    "dev": "unocss \"**/*.html\" -o main.css --watch & vite",
    "build": "unocss \"**/*.html\" -o main.css && vite build"
  }
}
SCRIPTEOF
jq '. + input' scripts.json _package.json > package.json
rm scripts.json _package.json

echo unocss.config
cat > unocss.config.js <<UNOEOF
// uno.config.ts
import {
    defineConfig,
    presetAttributify,
    presetTypography,
    presetUno
} from 'unocss'

import presetWebFonts from '@unocss/preset-web-fonts';

const fonts = presetWebFonts({
    provider: 'google', // default provider
    fonts: {
        header: [ {
            name: "Montserrat",
            weights: ['400', '700']
        } ],
        sans: [ { name: 'Inter' } ]
    }
})

export default defineConfig({
  presets: [
      presetAttributify(), // required when using attributify mode
      presetUno(), // required
      presetTypography(),
      fonts,
  ],
})
UNOEOF

echo main.js
cat > main.js <<MAINEOF
import '@unocss/reset/tailwind.css';
import './main.css';
MAINEOF

echo index.html
cat > index.html <<INDEXEOF
<html>
  <head>
    <title>Hello</title>
    <script src="main.js" type="module"></script>
    <meta name="viewport" content="width=device-width, initial-scale=1" />
  </head>
  <body >
    <div max-w-prose mx-auto prose>
      <h1 font-header text-4xl font-bold>Hello world</h1>

      <p font-sans>This is my text, and I really like it</p>
    </div>
  </body>
</html>
INDEXEOF

pwd
