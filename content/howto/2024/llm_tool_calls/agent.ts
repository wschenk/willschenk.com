// agent.ts
import { CoreMessage, CoreTool } from "ai";
import { modelFromString } from "./models";

export interface Context {
  model: string;
  messages: CoreMessage[];
  systemPrompt: string;
  tools?: Record<string, CoreTool<any, any>>;
}

export function startContext(
  model: string,
  systemPrompt: string,
  prompt: string
): Context {
  const ctx: Context = {
    model,
    systemPrompt,
    messages: [],
  };
  addMessage(ctx, prompt);
  return ctx;
}

export function addMessage(ctx: Context, message: string) {
  ctx.messages.push({
    role: "user",
    content: message,
  });
}

export function addTool(ctx: Context, name: string, tool: CoreTool<any, any>) {
  if (!ctx.tools) {
    ctx.tools = {};
  }
  ctx.tools[name] = tool;
}

export function makeGenerator(ctx: Context) {
  return {
    model: modelFromString(ctx.model),
    tools: ctx.tools,
    system: ctx.systemPrompt,
    messages: ctx.messages,
    maxToolRoundtrips: 5, // allow up to 5 tool roundtrips
  };
}
