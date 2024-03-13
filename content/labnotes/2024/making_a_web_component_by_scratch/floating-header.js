class FloatingHeader extends HTMLElement {
	connectedCallback () {
        // Create a MediaQueryList object
        const min_width = window.matchMedia("(min-width: 768px)")
        
        // Call listener function at run time
        this.setStyle(min_width);

        min_width.addEventListener("change", () => this.setStyle(min_width) )
    }

    setStyle(matcher) {
        let style = `
padding-top: 0.5rem;
padding-bottom: 0.5rem;
--un-bg-opacity: 1;
background-color: rgb(255 255 255 / var(--un-bg-opacity));
margin-left: auto;
margin-right: auto;
display: grid;
top: 0;
right: 0;
left: 0;
`
        // @media (min-width: 768px) {
        if( matcher.matches ) {
            style += `
padding-left: 1rem;
padding-right: 1rem;
border-width: 1px;
max-width: 1024px;
margin-top: 1rem;
margin-bottom: 1rem;
grid-auto-flow: column;
position: fixed;
border-style: solid;
border-radius: 0.75rem;
--un-border-opacity: 1;
border-color: rgb(226 232 240 / var(--un-border-opacity));
`
        }

        this.style.cssText = style;

        // Set the alignment of the children
        for (let i = 0; i < this.children.length; i++) {
            let align = 'start';
            if( matcher.matches ) {
                
                if( i == 0 ) {
                    align = 'start';
                } else if ( i == this.children.length - 1 ) {
                    align = 'end';
                } else {
                    align = 'center';
                }
            }
            this.children[i].style['justify-self'] = align;
        }
	}
}

customElements.define('floating-header', FloatingHeader );
