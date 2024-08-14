// ollama-image.ts
import { generateText } from "ai";
import { ollama } from "ollama-ai-provider";
import fs from "fs";

const model = ollama("llava:latest");

async function describeImage() {
  const result = await generateText({
    model: model,
    messages: [
      {
        role: "user",
        content: [
          { type: "text", text: "Describe the image in detail." },
          {
            type: "image",
            image: fs.readFileSync("./comic-cat.png").toString("base64"),
          },
        ],
      },
    ],
  });

  console.log(result);
}

const results = describeImage().catch((error) =>
  console.error("An error occurred:", error)
);
