import {extractUrlContent} from './extractor.js';

export async function crawl(link) {
    const seen = new Set();
    const tocrawl = new Set();
    const data = new Map();

    const baseUrl = link;
    while( link != undefined ) {
        tocrawl.delete(link)
        console.log( "dnling", link )
        seen.add( link );

        const content = await extractUrlContent(link);

        data.set( link, content ) 
        for( link of content.links ) {
            if( link.startsWith( baseUrl ) ) {
                if( !seen.has( link ) ) {
                    console.log( "adding", link )
                    tocrawl.add(link)
                }
            }
        }
        link = tocrawl.values().next().value
    }

    return data
}

//await crawl( 'https://support.tezlabapp.com/' )
//console.log( data )
//console.log( JSON.stringify( data, null, "" ) )
