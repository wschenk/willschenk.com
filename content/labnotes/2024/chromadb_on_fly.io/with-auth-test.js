// with-auth-test.js

import { ChromaClient } from "chromadb";

const client = new ChromaClient({
    path: "https://chromadb-on-fly-io.fly.dev",
    auth: {provider: "token", credentials: "test-token"}
});

const collection = await client.getOrCreateCollection({
    name: "my_authed_collection",
    metadata: {
        description: "My second collection"
    }
});

const collections = await client.listCollections();

console.log( "collections", collections );
