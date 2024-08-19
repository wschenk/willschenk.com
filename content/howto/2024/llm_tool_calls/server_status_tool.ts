// server_status_tool.ts
import { tool } from "ai";
import { addTool, Context } from "./agent";
import { z } from "zod";

export function addserver_status_tool(ctx: Context) {
  addTool(
    ctx,
    "server_status_tool",
    tool({
      description:
        "I would have liked to have a tool that shows the status of all running servers, but I don't have this capability. This tool should be able to display information such as server IP addresses, operating system versions, and current load averages.",
      parameters: z.object({
        argument: z
          .string()
          .describe("something something")
      }),
      execute: async ({ argument }) => {
        console.log("executing", "server_status_tool");
        return {
          argument,
        };
      },
    })
  );
}