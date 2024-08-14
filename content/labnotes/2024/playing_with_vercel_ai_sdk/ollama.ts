// ollama.ts

import { streamResponse } from "./streamResponse";
import { ollama } from "ollama-ai-provider";

const model = ollama("llama3:latest");

streamResponse(model).catch((error) =>
  console.error("An error occurred:", error)
);
