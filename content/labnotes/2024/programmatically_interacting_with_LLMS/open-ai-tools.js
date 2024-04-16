import 'dotenv/config'
import process from 'node:process'
import OpenAI from "openai";
import { weatherTool } from './weatherTool.js'
import { distanceTool } from './distanceTool.js'

const openai = new OpenAI()

async function main( content ) {
    const completion = await openai.chat.completions.create({
        messages: [
            {
                role: "system",
                content:
                `You are a help assistant.`
            },
            {
                role: "user",
                content
            }],
        model: "gpt-3.5-turbo",
        tools: [
            { type: "function", function: weatherTool},
            { type: "function", function: distanceTool}
        ]
    });

    console.log( content )
    console.log( JSON.stringify( completion.choices[0].message, null, "  " ) );
}

main( "how far is the drive from boston to brooklyn" )
main( "whats the weather on the north pole" )
