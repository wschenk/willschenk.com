import { OllamaEmbeddings } from "@langchain/community/embeddings/ollama"
import { Chroma } from "@langchain/community/vectorstores/chroma";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";
import { Document } from "langchain/document";
import { crawl } from "./importer-take-4.js";

const doc = new Document({ pageContent: "foo", metadata: { source: "1" } });

const collectionName = 'sofreshandclean';
const embeddingModel = 'nomic-embed-text';

// load the website
const siteData = await crawl( "https://support.tezlabapp.com/" )
const urls = Array.from(siteData.keys());

const docs = urls.map( (url) => {
    return new Document( {
        pageContent: siteData.get(url).textContent,
        metadata: { source: url }
    })
}).filter( (doc) => doc.pageContent )


console.log( "Making embeddings" );
//Get an instance of ollama embeddings
const ollamaEmbeddings = new OllamaEmbeddings({
    model: embeddingModel
});

// Chroma
const vectorStore = await Chroma.fromDocuments(
    docs,
    ollamaEmbeddings, {
        collectionName
    });

console.log( "stored" )
