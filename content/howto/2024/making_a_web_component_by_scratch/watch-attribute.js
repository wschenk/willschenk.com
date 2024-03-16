class WatchAttribute extends HTMLElement {
    static get observedAttributes() {
        return ["text"];
    }

    connectedCallback() {
        this.insertAdjacentHTML('beforeend', `<p></p>` )
        this.text = this.getAttribute( 'text' );
        this.updateText();
    }

    updateText() {
        let p = this.querySelector('p');
        if( p ) {
                p.innerHTML = this.text;
        }
    }
    
    attributeChangedCallback(name, oldValue, newValue) {
        if( name == 'text' ) {
            this.text = newValue;
            this.updateText();
        }
    }
}

customElements.define( 'watch-attribute', WatchAttribute );
