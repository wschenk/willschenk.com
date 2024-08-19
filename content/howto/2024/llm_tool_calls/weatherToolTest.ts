// weatherToolTest.ts
import { startContext } from "./agent";
import { streamResponse } from "./streamResponse";
import { addWeatherTool } from "./weatherTool";

const ctx = startContext(
  "ollama/llama3.1",
  "You are a helpful, respectful and honest assistant.",
  "Whats the weather in Tokyo?"
);

addWeatherTool(ctx);

streamResponse(ctx).catch(console.error);
