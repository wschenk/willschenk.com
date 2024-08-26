import {extractUrlContent} from './extractor.js';

  const urls = new Set()
  const docs = new Map()

  async function crawler( url, baseUrl =  null, level = 0 ) {
      if( baseUrl == null ) {
          baseUrl = url;
      }
      
      console.log( url, baseUrl, level )

      urls.add( url )
      const data = await extractUrlContent( url )

      docs.set( data.url, data )

      if( level <= 2 ) {
          for( const child of data.links ) {
              console.log( "does it", child, child.startsWith( baseUrl ) )
              if( child.startsWith( baseUrl ) ) {
                  if( !urls.has( child ) ) {
                      await crawler( child, baseUrl, level + 1 )
                  }
              }
          }
      }

      console.log( url, data.links )

      console.log( "urls", urls )
      return docs
  }

  const data = await crawler ( "https://support.tezlabapp.com/" )

//  console.log( data )
