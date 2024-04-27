class OllamaModels extends HTMLElement {
    connectedCallback() {
        this.state = {
            status: "",
            loading: true,
            tags: [],
            model: "mistral",
            api_key: "",
        }

        this.listTags();
        this.render();
    }

    get model() {
        return this.state.model;
    }

    get api_key() {
        return this.state.api_key;
    }

    async listTags() {
        const response = await fetch( "http://localhost:11434/api/tags" );
        const tags = await response.json();
        console.log( tags );
        this.state.tags = tags;
        this.state.loading = false;
        this.render();
    }

    render() {
        if( !this.state.loading ) {
            let h = `<sl-select hoist id="model" label="Model" help-text="Please select which model to run against.">`
            for( let model of this.state.tags.models ) {
                h += `<sl-option value="${model.name}">${model.name}</sl-option>`
                }
            h += `</sl-select>`

            this.innerHTML = h;

            this.querySelector( "sl-select" ).addEventListener( 'sl-change', (e) => {
                this.state.model = e.target.value;
            } );
        }
    }
}

customElements.define("ollama-models", OllamaModels );
