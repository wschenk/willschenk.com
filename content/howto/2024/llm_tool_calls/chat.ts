// chat.ts
import { addMessage, startContext } from "./agent";
import * as readline from "node:readline/promises";
import { streamResponse } from "./streamResponse";
import { addMetaTool } from "./metaTool";

async function chat() {
  const terminal = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  const userInput = await terminal.question("You: ");

  const ctx = startContext(
    "ollama/llama3.1",
    "You are a helpful, respectful and honest assistant.",
    userInput
  );

  addMetaTool(ctx);

  await streamResponse(ctx);

  while (true) {
    const userInput = await terminal.question("You: ");
    addMessage(ctx, userInput);
    await streamResponse(ctx);
    console.log("\n");
  }
}

chat().catch(console.error);
