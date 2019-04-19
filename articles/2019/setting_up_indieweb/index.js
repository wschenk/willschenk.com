const fetch = require( 'node-fetch' );
const microformat = require('microformat-node');

(async () => {
  const page = await fetch( 'https://willschenk.com' )
  const text = await page.text()
  const info = await microformat.get({html: text})

  console.log( JSON.stringify( info ) )
})()
