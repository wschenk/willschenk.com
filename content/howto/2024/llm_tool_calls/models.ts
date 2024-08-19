//models.ts

import { createOpenAI, openai } from "@ai-sdk/openai";
import { LanguageModel } from "ai";
import { ollama } from "ollama-ai-provider";

export function modelFromString(modelString: string): LanguageModel {
  const [provider, model] = modelString.split("/");
  if (provider === "ollama") {
    return ollama(model);
  } else if (provider === "openai") {
    return openai(model);
  } else if (provider === "groq") {
    const groq = createOpenAI({
      baseURL: "https://api.groq.com/openai/v1",
      // apiKey: process.env.GROQ_API_KEY,
    });

    return groq(model);
  }
  throw new Error(`Unknown model provider: ${provider}`);
}
