// streamResponse.ts
import { streamText } from "ai";
import { Context, makeGenerator } from "./agent";

export async function streamResponse(ctx: Context) {
  // This prints out the results to the screen
  let length = 0;
  function onChunk({ chunk }: { chunk: any }) {
    if (chunk.type === "text-delta") {
      if (length + chunk.textDelta.length > 80) {
        process.stdout.write("\n");
        length = 0;
      } else {
        length += chunk.textDelta.length;
      }
      process.stdout.write(chunk.textDelta);
    } else if (chunk.type === "tool-call") {
      console.log("Calling tool", chunk.toolName, chunk.args);
    } else if (chunk.type === "tool-result") {
      console.log("Tool result", chunk.toolName, chunk.result);
    } else {
      console.log("unknown", chunk.type, JSON.stringify(chunk, null, 2));
    }
  }

  // Sets up the model to run again if there are tool calls
  async function onFinish({
    //@ts-ignore
    text,
    //@ts-ignore
    toolCalls,
    //@ts-ignore
    toolResults,
    //@ts-ignore
    finishReason,
    //@ts-ignore
    usage,
  }) {
    // console.log("-----");
    // console.log("finishReason", finishReason);
    // console.log("toolCalls", toolCalls);
    // console.log("toolResults", toolResults);
    // console.log("usage", usage);
    // console.log("-----");
    // console.log("text", text);
    process.stdout.write("\n");

    if (finishReason === "tool-calls") {
      ctx.messages.push({
        role: "assistant",
        content: toolCalls,
      });

      ctx.messages.push({
        role: "tool",
        content: toolResults,
      });

      return await streamResponse(ctx);
    } else {
      ctx.messages.push({
        role: "assistant",
        content: text,
      });
      return "done";
    }
  }

  const generator = makeGenerator(ctx);

  //   @ts-ignore
  generator.onChunk = onChunk;

  // @ts-ignore
  generator.onFinish = onFinish;

  const result = await streamText(generator);

  // consume stream:
  for await (const textPart of result.textStream) {
    // Process each text part here
  }
}
