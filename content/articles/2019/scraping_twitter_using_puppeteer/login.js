/**
 * @name Twitter
 *
 * @desc Logs into Twitter. Provide your username and password as environment variables when running the script, i.e:
 * `TWITTER_USER=myuser TWITTER_PWD=mypassword node twitter.js`
 *
 */

const puppeteer = require('puppeteer');
const delay = require('delay');
const screenshot = 'timeline.png';

if( process.env.TWITTER_USER === undefined || process.env.TWITTER_PASSWORD === undefined ) {
  console.log( "Please set TWITTER_USER and TWITTER_PASSWORD to login" )
  process.exit(1);
}

(async () => {
  const browser = await puppeteer.launch({ headless: false })
  const page = await browser.newPage()
  await page.setViewport({ width: 1280, height: 800 })

  const navigationPromise = page.waitForNavigation()

  await page.goto('https://twitter.com/')
  console.log( "Got page");

  await page.waitForSelector('.LoginForm')
  console.log( "Found form" );

  await page.type('.LoginForm > .LoginForm-username > .text-input', process.env.TWITTER_USER)
  await page.type('.LoginForm > .LoginForm-password > .text-input', process.env.TWITTER_PASSWORD)
  await page.click('.LoginForm > .EdgeButton')
  console.log( "Logging in" );

  await navigationPromise
  await page.waitForSelector('#timeline')
  console.log( "Timeline loaded")

  console.log( "Pausing for media loading" )
  await delay( 5000 );

  console.log( "Taking screenshot" );
  await page.screenshot({ path: screenshot })
  await browser.close()
})()
