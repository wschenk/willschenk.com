import { compile } from "html-to-text";
import { RecursiveUrlLoader } from "langchain/document_loaders/web/recursive_url";

const url = "https://support.tezlabapp.com/"

const compiledConvert = compile({ wordwrap: 130 }); // returns (text: string) => string;

const loader = new RecursiveUrlLoader(url, {
    extractor: compiledConvert,
    maxDepth: 5,
    excludeDirs: ["https://js.langchain.com/docs/api/"],
});

const docs = await loader.load();

console.log( docs.length, "docs" )

for( let doc of docs ) {
    console.log( doc.metadata.source )
}
