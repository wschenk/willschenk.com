import 'dotenv/config'
import process from 'node:process'
import OpenAI from "openai";

const openai = new OpenAI()

async function main() {
  const completion = await openai.chat.completions.create({
    messages: [
        {
            role: "system",
            content:
            `You are a creative director at a marketing agency
helping clients brain storm interesting ideas.`
        },
        {
            role: "user",
            content: "Five cute names for a pet penguin"
        }],
      model: "gpt-3.5-turbo",
  });

  console.log(completion.choices[0].message.content);
}

main();
