import { TavilySearchResults } from "@langchain/community/tools/tavily_search";
import dotenv from "npm:dotenv";
dotenv.config();

const searchTool = new TavilySearchResults();

const toolResult = await searchTool.invoke("what is the weather in SF?");

console.log(toolResult);
