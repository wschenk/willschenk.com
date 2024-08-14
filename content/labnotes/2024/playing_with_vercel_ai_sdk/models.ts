//models.ts
import { openai } from "@ai-sdk/openai";
import { ollama } from "ollama-ai-provider";

export function getModel(model: string) {
  if (model === "gemma") {
    console.log("Using ollama", model);
    return ollama("gemma");
  }

  console.log("Using openai", model);
  return openai(model);
}
