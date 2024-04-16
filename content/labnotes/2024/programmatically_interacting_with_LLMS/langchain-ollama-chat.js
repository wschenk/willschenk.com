import { ChatOllama } from "@langchain/community/chat_models/ollama";
import { HumanMessage, AIMessage } from "@langchain/core/messages";
import { ChatMessageHistory } from "langchain/stores/message/in_memory";
import {
  ChatPromptTemplate,
  MessagesPlaceholder,
} from "@langchain/core/prompts";
import promptUser from './prompt.js'

const chat = new ChatOllama( { modal: 'gemma' } )

const prompt = ChatPromptTemplate.fromMessages([
  [
    "system",
    "You are a helpful assistant. Answer all questions to the best of your ability.",
  ],
  new MessagesPlaceholder("messages"),
]);

const chain = prompt.pipe(chat);

const messages = new ChatMessageHistory();


while( true ) {
    const message = await promptUser( "> " );

    if( message == "" ) {
        console.log( "Bye" )
        process.exit(0)
    }
    
    await messages.addMessage(
        new HumanMessage( message )
    )
    
    const responseMessage = await chain.invoke({
        messages: await messages.getMessages(),
    });

    await messages.addMessage( responseMessage )

    console.log( responseMessage.content )
}
