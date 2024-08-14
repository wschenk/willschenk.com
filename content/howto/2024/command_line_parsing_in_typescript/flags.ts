import {
  command,
  run,
  string,
  positional,
  restPositionals,
  boolean,
  flag,
} from "cmd-ts";

const app = command({
  name: "app",
  args: {
    debug: flag({
      long: "debug",
      short: "d",
      type: boolean,
      description: "Enable debug mode",
      defaultValue: () => false,
    }),
    first: positional({ type: string, displayName: "some arg" }),
    rest: restPositionals({ type: string, displayName: "rest" }),
  },
  handler: ({ debug, first, rest }) => {
    console.log({ debug });
    console.log({ first });
    console.log({ rest });
  },
});

run(app, process.argv.slice(2));
