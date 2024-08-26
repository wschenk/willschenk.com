import { compile } from "html-to-text";
import { CheerioWebBaseLoader } from "langchain/document_loaders/web/cheerio";

const url = "https://support.tezlabapp.com/"

const loader = new CheerioWebBaseLoader(
    url
);
const compiledConvert = compile({ wordwrap: 130 }); // returns (text: string) => string;

const docs = await loader.load();

console.log(docs)
console.log( docs.length, "docs" )

for( let doc of docs ) {
    console.log( doc.metadata.source )
}
