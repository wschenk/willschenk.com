import ollama from "ollama";
import { ChromaClient } from "chromadb";

const collectionName = "butterysmooth"
const embeddingModel = 'nomic-embed-text';

const chroma = new ChromaClient({ path: "http://localhost:8000" });
const collection = await chroma.getOrCreateCollection({
    name: collectionName,
    metadata: { "hnsw:space": "cosine" } });

//const query = "who condescends?"
const query = "which character is more prideful and why?"

// Generate embedding based upon the query
const queryembed = (await ollama.embeddings({
    model: embeddingModel,
    prompt: query })).embedding;

const relevantDocs = (await collection.query({
    queryEmbeddings: [queryembed], nResults: 10 })).documents[0].join("\n\n")
const modelQuery = `${query} - Answer that question using the following text as a resource: ${relevantDocs}`
//console.log( "querying with text", modelQuery )

const stream = await ollama.generate({ model: "mistral", prompt: modelQuery, stream: true });

for await (const chunk of stream) {
  process.stdout.write(chunk.response)
}
console.log( "")
