// generateResponse.ts

import { generateText, LanguageModel } from "ai";
import { Context, makeGenerator } from "./agent";

export async function generateResponse(ctx: Context) {
  const generator = makeGenerator(ctx);
  const result = await generateText(generator);
  for await (const message of result.responseMessages) {
    ctx.messages.push(message);
  }
  // console.log("result", JSON.stringify(result, null, 2));
  // console.log(ctx.messages[ctx.messages.length - 1].content);
  return result;
}
