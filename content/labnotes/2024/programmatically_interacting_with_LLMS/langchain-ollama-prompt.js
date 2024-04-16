import { ChatOllama } from "@langchain/community/chat_models/ollama";
import runPrompt from "./langchain-prompt.js"

const chatModel = new ChatOllama({
    baseUrl: "http://localhost:11434", // Default value
    model: "mistral",
});

const answer = await runPrompt( chatModel, "Five cute names for a pet penguin" );

console.log( answer )
