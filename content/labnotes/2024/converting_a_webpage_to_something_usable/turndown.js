// turndown.js
import TurndownService from 'turndown';
import { JSDOM } from 'jsdom';

export default async function extractMarkdown(url) {
    const response = await fetch( url );
    const doc = await response.text();

    const turndownService = TurndownService();

    const markdown = turndownService.turndown(doc);
    return markdown;
}

const markdown = await extractMarkdown( "https://willschenk.com/fragments/2024/i_need_a_trigger_warning/" )
console.log( markdown )
