const puppeteer = require('puppeteer');
const mkdirp = require( 'mkdirp' );
const fs = require( 'fs' );

const screenname = process.env.TWITTER_USER;
const outdir = `output/${screenname}`;

if( screenname === undefined  ) {
  console.log( "Please set TWITTER_USER" )
  process.exit(1);
}

// Create the directory we'll write output to
mkdirp.sync( outdir );

(async () => {
  const browser = await puppeteer.launch({ headless: true });
  const page = await browser.newPage();

  const navigationPromise = page.waitForNavigation()

  await page.goto(`https://twitter.com/${screenname}`)
  console.log( "Got page");

  await navigationPromise
  await page.waitForSelector('#timeline')
  console.log( "Timeline loaded")

  const profile = await page.evaluate( () => {
    var profile = {}
    profile.name = document.querySelector( ".ProfileHeaderCard-name" ).textContent.trim();
    profile.screenname = document.querySelector( ".ProfileHeaderCard-screenname").textContent.trim();
    profile.bio = document.querySelector( ".ProfileHeaderCard-bio").textContent.trim();
    profile.location = document.querySelector( ".ProfileHeaderCard-locationText").textContent.trim();
    profile.url = document.querySelector( ".ProfileHeaderCard-url a" ).title;
    profile.joined = document.querySelector( ".ProfileHeaderCard-joinDateText" ).dataset.originalTitle;
    profile.avatar_url = document.querySelector( ".ProfileAvatar-image" ).src;
    profile.canopy_url = document.querySelector( ".ProfileCanopy-headerBg img").src;

    profile.tweets = document.querySelector( ".ProfileNav-item--tweets .ProfileNav-value" ).dataset.count;
    profile.following = document.querySelector( ".ProfileNav-item--following .ProfileNav-value" ).dataset.count;
    profile.followers = document.querySelector( ".ProfileNav-item--followers .ProfileNav-value" ).dataset.count;
    profile.likes = document.querySelector( ".ProfileNav-item--favorites .ProfileNav-value" ).dataset.count;

    return profile;
  })

  console.log("profile",profile);

  // Writing file
  fs.writeFileSync( `${outdir}/profile.json`, JSON.stringify( profile ) );

  // console.log( "Taking screenshot" );
  // await page.screenshot({ path: `${outdir}/profile.png` })
  await browser.close()
})()
