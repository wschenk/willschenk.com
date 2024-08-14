// openai-image.ts
import { openai } from "@ai-sdk/openai";
import { generateText } from "ai";
import dotenv from "dotenv";

dotenv.config();

const model = openai("gpt-4o");

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
            image:
              "https://github.com/vercel/ai/blob/main/examples/ai-core/data/comic-cat.png?raw=true",
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
