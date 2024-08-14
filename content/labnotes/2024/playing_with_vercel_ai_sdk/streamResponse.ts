// streamResponse.ts
import { CoreMessage, LanguageModel, streamText } from "ai";

export async function streamResponse(model: LanguageModel) {
  const messages: CoreMessage[] = [];

  messages.push({ role: "user", content: "Five penguin names" });
  const result = await streamText({
    model: model,
    system: `You are a helpful, respectful and honest assistant.`,
    messages,
  });

  let fullResponse = "";
  process.stdout.write("\nAssistant: ");
  for await (const delta of result.textStream) {
    fullResponse += delta;
    process.stdout.write(delta);
  }
  process.stdout.write("\n\n");
}
