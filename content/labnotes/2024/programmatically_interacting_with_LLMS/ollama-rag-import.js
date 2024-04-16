import ollama from "ollama";
import fs from 'node:fs';
import sentencize from '@stdlib/nlp-sentencize';
import { ChromaClient } from "chromadb";

const fileName = "1342-0.txt";
const collectionName = "butterysmooth";
const embeddingModel = 'nomic-embed-text';

// Load the source file
const file = fs.readFileSync( fileName, 'utf8' )
const sentences = sentencize(file)

console.log( "Loaded", sentences.length, "sentences" )

// Setup Chroma

const chroma = new ChromaClient({ path: "http://localhost:8000" });
await chroma.deleteCollection({
    name: collectionName
});

console.log( "Creating collection", collectionName );

const collection = await chroma.getOrCreateCollection({
    name: collectionName,
    metadata: { "hnsw:space": "cosine" }
});

// Generate the embeddings
for( let i = 0; i < sentences.length; i++ ) {
    const s = sentences[i];

    const embedding = (await ollama.embeddings( {
        model: embeddingModel
        prompt: sentences[i]
    })).embedding

    await collection.add( {
        ids: [s + i],
        embeddings: [embedding],
        metadatas: {
            source: fileName
        },
        documents: [s]
    })

    if( i % 100 == 0 ) {
        process.stdout.write(".")
    }
}

console.log("");
