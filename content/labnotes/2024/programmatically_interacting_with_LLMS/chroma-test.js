import { ChromaClient } from "chromadb";

const client = new ChromaClient({
    path: "http://localhost:8000"
});

let collections = await client.listCollections();

console.log( "collections", collections );

const collection = await client.getOrCreateCollection({
    name: "my_collection",
    metadata: {
        description: "My first collection"
    }
});

collections = await client.listCollections();

console.log( "collections", collections );
