//complete.ts
import fs from "node:fs";
import { CoreMessage, streamText } from "ai";
import { getModel } from "./models.ts";
import process from "node:process";

interface Context {
  model: string;
  messages: CoreMessage[];
}

export async function writeContext(context: Context) {
  let contextJson = JSON.stringify(context, null, 2);

  await fs.writeFile("context.json", contextJson, (err) => {
    if (err) {
      console.log("Error writing file:", err);
    } else {
      console.log("Successfully wrote file");
    }
  });
}

export async function readContext(): Promise<Context> {
  return new Promise<Context>((resolve, reject) => {
    fs.readFile("context.json", (err, data) => {
      if (err) {
        reject(err);
      } else {
        try {
          const parsedContext = JSON.parse(data.toString());
          resolve(parsedContext);
        } catch (parseError) {
          reject(parseError);
        }
      }
    });
  });
}

export async function streamResponse(
  modelName: string,
  prompt: string,
  context: boolean
) {
  let workingContext: Context = {
    model: modelName,
    messages: [],
  };

  if (context) {
    workingContext = await readContext();
  }

  workingContext.messages.push({ role: "user", content: prompt });

  const model = getModel(workingContext.model);
  const result = await streamText({
    model,
    system: `You are a helpful, respectful and honest assistant.`,
    messages: workingContext.messages,
  });

  let fullResponse = "";
  process.stdout.write("\nAssistant: ");
  for await (const delta of result.textStream) {
    fullResponse += delta;
    process.stdout.write(delta);
  }
  process.stdout.write("\n\n");

  workingContext.messages.push({ role: "assistant", content: fullResponse });

  writeContext(workingContext);
}
