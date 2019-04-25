/**
 * @name Twitter
 *
 * @desc Logs into Twitter. Provide your username and password as environment variables when running the script, i.e:
 * `TWITTER_USER=myuser TWITTER_PWD=mypassword node twitter.js`
 *
 */

const puppeteer = require('puppeteer');
const screenshot = 'twitter.png';

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
  await navigationPromise
const cookies = await page.cookies();
console.log( cookies );

  await page.waitForSelector('#timeline')

  await page.screenshot({ path: screenshot })
  await browser.close()
})()

