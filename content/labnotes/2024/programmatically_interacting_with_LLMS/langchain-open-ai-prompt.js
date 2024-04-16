import 'dotenv/config'
import { ChatOpenAI } from "@langchain/openai";
import runPrompt from "./langchain-prompt.js"

const chatModel = new ChatOpenAI();
const answer = await runPrompt( chatModel, "Five cute names for a pet penguin" );

console.log( answer )
