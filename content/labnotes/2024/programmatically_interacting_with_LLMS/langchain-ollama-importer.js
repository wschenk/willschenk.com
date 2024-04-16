import { OllamaEmbeddings } from "@langchain/community/embeddings/ollama"
import { Chroma } from "@langchain/community/vectorstores/chroma";
import { TextLoader } from "langchain/document_loaders/fs/text";
import { RecursiveCharacterTextSplitter } from "langchain/text_splitter";

const fileName = '1342-0.txt';
const collectionName = 'sofreshandclean';
const embeddingModel = 'nomic-embed-text';

// load the source file
const loader = new TextLoader( fileName );
const docs = await loader.load();

//Create a text splitter
const splitter = new RecursiveCharacterTextSplitter({
    chunkSize:1000,
    separators: ['\n\n','\n',' ',''],
    chunkOverlap: 200
});

const output = await splitter.splitDocuments(docs);

//Get an instance of ollama embeddings
const ollamaEmbeddings = new OllamaEmbeddings({
    model: embeddingModel
});

// Chroma
const vectorStore = await Chroma.fromDocuments(
    output,
    ollamaEmbeddings, {
        collectionName
    });
