// metaTool.ts
import { tool } from "ai";
import { addTool, Context } from "./agent";
import { z } from "zod";
import * as fs from "fs";

export function addMetaTool(ctx: Context) {
  addTool(
    ctx,
    "meta_tool",
    tool({
      description: `Whenever the query from the user exceeds your capabilities, you can
call this tool to request the development of another tool. Our team
will review these logs and maybe develop the tool you requested so
your capabilities will improve`,
      parameters: z.object({
        tool_name: z
          .string()
          .describe("name of the tool that you'd like to use"),
        tool_description: z.string().describe(
          `A description of the tool you would have like to have to correctly
answer to the user and how their query was exceeding your current
capabilities`
        ),
      }),
      execute: async ({ tool_name, tool_description }) => {
        makeToolRequest(tool_name, tool_description);
        console.log("tool call for", tool_name);
        console.log("tool call for", tool_description);
        return {
          tool_name,
          tool_description,
        };
      },
    })
  );
}

function makeToolRequest(tool_name: string, tool_description: string) {
  console.log("templating a tool called", tool_name);
  console.log("with the following description", tool_description);

  const file = `${tool_name}.ts`;
  if (fs.existsSync(file)) {
    console.log("file already exists", file);
  } else {
    console.log("writing file", file);
    fs.writeFileSync(
      file,
      `// ${tool_name}.ts
import { tool } from "ai";
import { addTool, Context } from "./agent";
import { z } from "zod";

export function add${tool_name}(ctx: Context) {
  addTool(
    ctx,
    "${tool_name}",
    tool({
      description:
        "${tool_description}",
      parameters: z.object({
        argument: z
          .string()
          .describe("something something")
      }),
      execute: async ({ argument }) => {
        console.log("executing", "${tool_name}");
        return {
          argument,
        };
      },
    })
  );
}`
    );
  }
}
