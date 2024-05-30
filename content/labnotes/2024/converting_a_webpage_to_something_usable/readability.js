// readability.js
import TurndownService from 'turndown';
import { Readability } from '@mozilla/readability';
import { JSDOM } from 'jsdom';


export default async function extractText(url) {
    const doc = await JSDOM.fromURL(url);
    const reader = new Readability(doc.window.document, {keepClasses:false, classesToPreserve: ['BLOCKQUOTE']});
    const article = reader.parse();

    console.log( article.content );
    
    const turndownService = TurndownService();
    const markdown = turndownService.turndown(article.content);

    return markdown;
}

const text = await extractText( "https://willschenk.com/fragments/2024/i_need_a_trigger_warning/" )

console.log( text )
