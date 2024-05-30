// no-auth-test.js

import { ChromaClient } from "chromadb";

const client = new ChromaClient({
    path: "https://chromadb-on-fly-io.fly.dev"
});

const collection = await client.getOrCreateCollection({
    name: "my_collection",
    metadata: {
        description: "My first collection"
    }
});

const collections = await client.listCollections();

console.log( "collections", collections );
