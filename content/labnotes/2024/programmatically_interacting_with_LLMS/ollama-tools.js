import ollama from 'ollama';
import promptUser from './prompt.js';
import makePrompt from './ollama-tool-prompt.js'
import { weatherTool } from './weatherTool.js'
import { distanceTool } from './distanceTool.js'

const model = 'mistral'

const messages = [
    {
        "role": "system",
        "content": makePrompt( [distanceTool, weatherTool] )
    }
]

while( true ) {
    const content = await promptUser( "> " );

    if( content == "" ) {
        console.log( "Bye" )
        process.exit(0);
    }
    messages.push( { role: "user", content } )

    const prompt = makePrompt( [distanceTool, weatherTool] ) + content
    
    const output = await ollama.generate({ model, prompt })
    console.log( output.response )

    console.log( JSON.parse( output.response ) )
}
