import { Ollama } from "@langchain/community/llms/ollama";
import { ChatOpenAI } from "@langchain/openai";

import {
    ChatPromptTemplate,
} from "@langchain/core/prompts";
import { StringOutputParser } from "@langchain/core/output_parsers"
import { marked } from 'marked';

class GenerateResponse extends HTMLElement {
    connectedCallback() {
        this.state = {
            llm: this.getAttribute( 'llm' ) || 'ollama',
            api_key: this.getAttribute( 'api_key' ),
            status: "",
            model: this.getAttribute( "model" ) || "mistral",
            response: "",
            query: this.getAttribute( "query" )
        }

        this.doQuery();
        this.render();
    }

    async doQuery() {
        const model = this.state.model;
        this.state.status = `generating response from ${model}`
        this.render();

        let chatModel = new Ollama( { model } );
        if( this.state.llm == 'openAI' ) {
            chatModel = new ChatOpenAI( {apiKey: this.state.api_key} );
        }

        const messages = [
            [
                "system",
                "You are a helpful AI assistant"
            ]
        ];

        // Get the history
        const exchange = document.querySelectorAll( "generate-response" )
        for( let m of exchange ) {
            if( m.state.query ) {
                messages.push( [ "user", m.state.query ] )
            }

            if( m.state.response != "" ) {
                messages.push( [ "ai", m.state.response ] )
            }
        }

        const prompt = ChatPromptTemplate.fromMessages(messages);
        const outputParser = new StringOutputParser();
        const llmChain = prompt.pipe(chatModel).pipe(outputParser);
        const answer = await llmChain.stream({
            input: this.state.query
        });

        for await (const chunk of answer) {
            this.state.response = this.state.response + chunk;
            this.render()
        }

        this.state.status = "";
        this.render();
    }
    
    render() {
        let h = ""

        h += `<h2 font-header text-2xl>${this.state.query}</h2>`
        
        if( this.state.response == "" ) {
            h += `<sl-progress-bar indeterminate py-2></sl-progress-bar>`
        }

        if( this.state.status != "" ) {
            h += `<p>${this.state.status}</p>`
        }

        if( this.state.response != "" ) {
            h += `<div>`
            h += marked.parse( this.state.response )
            h += `</div>`
        }

        this.innerHTML = h;
    }
}

customElements.define("generate-response", GenerateResponse );
