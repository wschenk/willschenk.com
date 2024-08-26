import { JSDOM } from 'jsdom';
import { compile } from 'html-to-text';

const compiledConvert = compile();

export async function extractUrlContent(url) {
    try {
        const dom = await JSDOM.fromURL(url);
        
        const links = [...dom.window.document.querySelectorAll('a')]
                      .map(link => link.href)
                      .filter(href => href.startsWith('http') || href.startsWith('www'));

        // Use Readability to parse the page
        //const reader = new Readability(dom.window.document);
        //const article = reader.parse();

        const article = compiledConvert( dom.window.document.body.innerHTML);

        // Return the results
        return {
            url: url,
            links: links,
            textContent: article 
        };
    } catch (error) {
        console.error('Error extracting content:', error);
        return {
            url: url,
            links: [],
            textContent: ''
        };
    }
}
