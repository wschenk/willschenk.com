// weatherTool.ts
import { tool } from "ai";
import { z } from "zod";
import { addTool, Context } from "./agent";

export function addWeatherTool(ctx: Context) {
  addTool(
    ctx,
    "weather",
    tool({
      description: "Get the weather in a location",
      parameters: z.object({
        location: z.string().describe("The location to get the weather for"),
      }),
      execute: async ({ location }) => {
        console.log("tool call for", location);
        return {
          location,
          temperature: 72 + Math.floor(Math.random() * 21) - 10,
        };
      },
    })
  );
}
