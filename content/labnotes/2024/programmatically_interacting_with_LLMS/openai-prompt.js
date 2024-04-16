import { ChatOllama } from "@langchain/community/chat_models/ollama";
import { ChatPromptTemplate } from "@langchain/core/prompts";
import { StringOutputParser } from "@langchain/core/output_parsers";

const chatModel = new ChatOllama({
    baseUrl: "http://localhost:11434", // Default value
    model: "mistral",
});

const prompt = ChatPromptTemplate.fromMessages([
    [
        "system",
        "You are a creative director at a marketing agency helping clients brain storm interesting ideas."],
    [
        "user",
        "{input}"
    ],
]);

const outputParser = new StringOutputParser();

const chain = prompt.pipe(chatModel);

const llmChain = prompt.pipe(chatModel).pipe(outputParser);

const answer = await llmChain.invoke({
    input: "Five cute names for a pet penguin"
});

console.log( answer )
