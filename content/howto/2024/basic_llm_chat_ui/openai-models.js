class OpenaiModels extends HTMLElement {

    connectedCallback() {
        this.state = {
            api: localStorage.getItem( "openai_api_key" ),
            models: ['gpt-4-turbo', 'gpt-4', 'gpt-3.5-turbo', 'gpt-3.5'],
            model: 'gpt-4-turbo'
        }

        this.render();
    }

    get model() {
        return this.state.model;
    }
    
    get api_key() {
        return this.state.api;
    }
    
    render() {
        let h = `<sl-input id='api' label="API KEY" value="${this.state.api}"></sl-input>`;

        h = h + `<sl-select hoist id="model" label="Model" help-text="Please select which model to run against.">`
        for( let model of this.state.models ) {
            h += `<sl-option value="${model}">${model}</sl-option>`
        }
        h += `</sl-select>`

        this.innerHTML = h;

        this.querySelector( 'sl-input' ).addEventListener( "sl-input", (e) => {
            this.state.api = e.target.value;
            localStorage.setItem( "openai_api_key", this.state.api );
            console.log( this.state.api );
        } );

        this.querySelector( "sl-select" ).addEventListener( 'sl-change', (e) => {
            this.state.model = e.target.value;
        } );

    }
}

customElements.define( "openai-models", OpenaiModels );
