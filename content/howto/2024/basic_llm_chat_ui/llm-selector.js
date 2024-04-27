class LLMSelector extends HTMLElement {
    get llm() {
        return this.state.llm;
    }

    get model() {
        const model = this.querySelector( `#${this.state.llm}` );
        return model.model;
    }

    get api_key() {
        const model = this.querySelector( `#${this.state.llm}` );
        return model.api_key;
    }
    
    connectedCallback() {
        this.state = {llm: 'ollama'}
        
        this.innerHTML = `
<sl-tab-group>
  <sl-tab slot="nav" panel="ollama">Ollama</sl-tab>
  <sl-tab slot="nav" panel="openAI">OpenAI</sl-tab>

  <sl-tab-panel name="ollama"><ollama-models id="ollama"></ollama-models></sl-tab-panel>
  <sl-tab-panel name="openAI"><openai-models id="openAI"></openai-models></sl-tab-panel>
</sl-tab-group>`

        this.querySelector( "sl-tab-group" ).addEventListener( "sl-tab-show", (e) => {
            this.state.llm = e.detail.name;
        })
    }

}

customElements.define("llm-selector", LLMSelector );
