import { command, flag, option, restPositionals, run } from "cmd-ts";
import { streamResponse } from "./complete.ts";
import dotenv from "dotenv";
import process from "node:process";
dotenv.config();

const app = command({
  name: "complete",
  args: {
    model: option({
      long: "model",
      short: "m",
      description: "The model to use",
      defaultValue: () => "gemma",
    }),
    context: flag({
      long: "context",
      short: "c",
      description: "The context to use",
    }),
    prompt: restPositionals({
      description: "The prompt to use",

      displayName: "prompt",
    }),
  },
  handler: async ({ model, prompt, context }) => {
    if (prompt.length === 0) {
      console.error("No prompt provided");
      process.exit(1);
    }

    await streamResponse(model, prompt.join(" "), context);
  },
});

run(app, process.argv.slice(2)).catch(console.error);
