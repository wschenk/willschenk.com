import promptUser from './prompt.js';
import ollama from 'ollama';

const model = 'mistral'

const messages = [
    {
        role: "system",
        content: "You are a helpful AI agent"
    }
]

while( true ) {
    const content = await promptUser( "> " );

    if( content == "" ) {
        console.log( "Bye" );
        process.exit(0);
    }
    
    messages.push( { role: "user", content } )
    
    const response = await ollama.chat( {
        model,
        messages,
        stream: true } )
    
    let cc = 0

    let text = ""
    for await (const part of response) {
        cc = cc + part.message.content.length
        if( cc > 80 ) {
            process.stdout.write( "\n" );
            cc = part.message.content.length
        }
        process.stdout.write( part.message.content )
        text = text + part.message.content
    }
    process.stdout.write( "\n" );

    messages.push( { role: "assistant", content: text } )
}
