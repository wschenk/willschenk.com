import {
  command,
  run,
  string,
  restPositionals,
  option,
  subcommands,
} from "cmd-ts";

const models_list = command({
  name: "list",
  description: "List all models",
  args: {},
  handler: () => {
    console.log("list");
  },
});

const models_subcommand = subcommands({
  name: "models",
  description: "Manage models",
  cmds: {
    list: models_list,
  },
});

const run_prompt = command({
  name: "run",
  description: "Run a prompt",
  args: {
    model: option({
      long: "model",
      short: "m",
      description: "The model to use",
      defaultValue: () => "gpt-4o",
    }),
    prompt: restPositionals({ type: string, displayName: "prompt" }),
  },
  handler: ({ model, prompt }) => {
    console.log("model", model);
    console.log("prompt", prompt.join(" "));
  },
});

const app = subcommands({
  name: "app",
  cmds: { run: run_prompt, models: models_subcommand },
});

run(app, process.argv.slice(2)).catch(console.error);
