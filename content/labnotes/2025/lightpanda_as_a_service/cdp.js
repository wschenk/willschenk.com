'use strict'
 
import puppeteer from 'puppeteer-core'
 
// use browserWSEndpoint to pass the Lightpanda's CDP server address.
const browser = await puppeteer.connect({
  browserWSEndpoint: "ws://127.0.0.1:9222",
})
 
// The rest of your script remains the same.
const context = await browser.createBrowserContext()
const page = await context.newPage()
 
// Dump all the links from the page.
await page.goto('https://wikipedia.com/')
 
const links = await page.evaluate(() => {
  return Array.from(document.querySelectorAll('a')).map(row => {
    return row.getAttribute('href')
  })
})
 
console.log(links)
 
await page.close()
await context.close()
await browser.disconnect()
