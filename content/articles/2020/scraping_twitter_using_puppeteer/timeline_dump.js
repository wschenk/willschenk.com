/*
  Loads the main profile page and writes out the rendered HTML
*/

const puppeteer = require('puppeteer');
const mkdirp = require( 'mkdirp' );
const delay = require('delay');
const fs = require( 'fs' );

if( process.env.TWITTER_USER === undefined ) {
    console.log( "Please set TWITTER_USER" );
    process.exit(1);
}

const screenname = process.env.TWITTER_USER;
const outdir = `output/${screenname}`;

delay(50000).then( () => {
    console.log( "timeout after 20 seconds!" );
    process.exit(1)
});

mkdirp.sync( outdir );

(async () => {
    const browser = await puppeteer.launch({ headless: false })
    const page = await browser.newPage()
    
    const navigationPromise = page.waitForNavigation()

    await page.goto(`https://twitter.com/${screenname}`)
    console.log( "Got page");

    const bodyHandle = await page.$('html');
    const html = await page.evaluate(body => body.innerHTML, bodyHandle);
    fs.writeFileSync( `${outdir}/timeline.html`, html );
    console.log( `Wrote ${outdir}/timeline.html` );

    process.exit(0);
})()
