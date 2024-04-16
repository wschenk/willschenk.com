import { OllamaFunctions } from "langchain/experimental/chat_models/ollama_functions";
import { HumanMessage } from "@langchain/core/messages";
import { weatherTool } from "./weatherTool.js";
import { distanceTool } from "./distanceTool.js";

const model = new OllamaFunctions({
    temperature: 0.1,
    model: "mistral",
} )
      .bind( {
          functions: [
              weatherTool,
              distanceTool
          ] } )

let response = await model.invoke([
    new HumanMessage({
        content: "What's the weather in Boston?",
    }),
]);

console.log(response.additional_kwargs);

response = await model.invoke([
    new HumanMessage({
        content: "How far is it to drive from portland maine to the same city in oregon?",
    }),
]);

console.log(response.additional_kwargs);
