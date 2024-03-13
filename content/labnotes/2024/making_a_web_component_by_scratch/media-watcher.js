class MediaWatcher extends HTMLElement {
    constructor() {
        super();

        this.match = this.getAttribute( "match" );
        this.unmatch = this.getAttribute( "unmatch" );
    }
    
    connectedCallback() {
        this.min_width = window.matchMedia("(min-width: 768px)");
        this.min_width.addEventListener( "change",
                                         () => this.setStyle( this.min_width ) );
        this.setStyle( this.min_width );
    }

    setStyle(matcher) {
        this.style.cssText = matcher.matches ? this.match : this.unmatch;
    }
}

customElements.define( 'media-watcher', MediaWatcher )
