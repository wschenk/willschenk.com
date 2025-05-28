import { Defuddle } from "defuddle/node";
import fs from "fs";

import { exec } from "child_process";
import { promisify } from "util";

const execAsync = promisify(exec);

async function parse(url) {
    // Execute lightpanda command and get output
    const { stdout } = await execAsync(`./lightpanda --dump ${url}`);

    const result = await Defuddle(stdout, url, {
        // debug: true, // Enable debug mode for verbose logging
        markdown: true, // Convert content to markdown
    });

    return result;
}

const url = process.argv[2];
if (!url) {
    console.error("Please provide a URL as the first argument");
    process.exit(1);
}

const result = await parse(url).then((result) => {
    console.log("title:",result.title);
    console.log("author:",result.author);
    console.log("content")
    console.log(result.content);
    
    
    process.exit(0);
});
