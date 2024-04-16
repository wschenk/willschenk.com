import { ChatPromptTemplate } from "@langchain/core/prompts";
import { StringOutputParser } from "@langchain/core/output_parsers";

export default async function runPrompt( chatModel, input ) {
    const prompt = ChatPromptTemplate.fromMessages([
        [
            "system",
            `You are a creative director at a marketing
agency helping clients brain storm interesting ideas.`],
        [
            "user",
            "{input}"
        ],
    ]);
    
    const outputParser = new StringOutputParser();
    const llmChain = prompt.pipe(chatModel).pipe(outputParser);
    const answer = await llmChain.invoke({
        input
    });

    return answer
}
