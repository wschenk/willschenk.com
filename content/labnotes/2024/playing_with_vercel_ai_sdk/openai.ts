// openai.ts

import { streamResponse } from "./streamResponse";
import { openai } from "@ai-sdk/openai";
import dotenv from "dotenv";

dotenv.config();

const model = openai("gpt-4o");

streamResponse(model).catch((error) =>
  console.error("An error occurred:", error)
);
