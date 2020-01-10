const puppeteer = require('puppeteer');
const delay = require('delay');
const fs = require( 'fs' );

if( process.env.TWITTER_USER === undefined ) {
    console.log( "Please set TWITTER_USER" );
    process.exit(1);
}

const screenname = process.env.TWITTER_USER;

delay(20000).then( () => {
    console.log( "timeout after 20 seconds!" );
    process.exit(1)
});
		  

(async () => {
    const browser = await puppeteer.launch({ headless: false })
    const page = await browser.newPage()
    
    const navigationPromise = page.waitForNavigation()

    await page.goto(`https://twitter.com/${screenname}`)
    console.log( "Got page");

    // Wait for the timeline to render
    let timeline = await page.waitForSelector('#timeline');
    console.log( "Timeline loaded" )

    let lgr = console;
    
    const tweets = await page.evaluate( () => {
	// Simple utility function that returns the value if it is found
	function v( data ) {
	    if( data ) {
		return data.value;
	    }
	    return "";
	}

	// Loop through all the tweets on the page
	let tweets = [];
	document.querySelectorAll( '.tweet' ).forEach( tweet => {
	    let a = tweet.attributes;
	    // Main metadata
	    let d = {
		id: v(a['data-tweet-id']),
		permalink: v(a['data-permalink-path']),
		screenname: v(a['data-screen-name']),
		youfollow: v(a['data-you-follow']),
		followsyou: v(a['data-follows-you']),
		hascards: v(a['data-has-cards'])
	    }

	    // Get the unix time
	    let time = tweet.querySelector(".tweet-timestamp");
	    if( time[0] ) {
		d.time = v(time[0].attributes['data-time']);
	    }

	    // Get the tweet text
	    let text = tweet.querySelector( ".tweet-text" );
	    d.text = text.innerText;

	    // Pull the full links out of the tweet
	    let links = [];
	    text.querySelectorAll( "a.twitter-timeline-link" ).forEach( link => {
		links.push( v(link.attributes["data-expanded-url"] ) );
	    })
	    d.links = links;

	    // Pull out posted images
	    let media = [];
	    tweet.querySelectorAll( ".AdaptiveMedia-container img" ).forEach( img => {
		media.push( img.src );
	    } )

	    d.media = media;
	    
	    
	    tweets.push( d )
	});
	
	return tweets
	
    } );

    console.log( "tweets", tweets );

    process.exit(0);
})()
