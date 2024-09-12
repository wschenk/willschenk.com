import {deflateToBase64, inflateFromBase64} from './compress.js'

class SlidesHolder extends HTMLElement {
    constructor() {
        super();
        // Treat the whole thing as a template
        
        this.template = `${this.innerHTML}`;
        
        const header = `<header>
 <button id="saveState">Save State</button>
 <button id="newSlide">New Slide</button>
 <button id="nextSlide">Next Slide</button>
 </header>`;

        // default
        let sections = [`<section>${this.template}</section>`];

        // load from cache if exists
        const hash = window.location.hash;
        if( hash != '' ) {
            sections = [];
            
            const state = JSON.parse(inflateFromBase64( hash.substring( 1 ) ))
            for( let section of state ) {
                let frag = `<section><content-slide>`

                for( let elem of section ) {
                    frag += `<${elem.nodeName}>${elem.text}</${elem.nodeName}>`
                }
                
                frag += "</content-slide></section>"

                sections.push( frag );
            }
        }
                
        this.innerHTML = `${header}${sections.join( "\n")}`
    }
    
    connectedCallback() {
        this.querySelector( "#nextSlide" ).
            addEventListener( "click", () => this.nextSlide() );
        this.querySelector( "#newSlide" ).
            addEventListener( "click", () => this.newSection() );
        this.querySelector( "#saveState" ).
            addEventListener( "click", () => this.getState() );
    }
    
    getState() {
        const state = []
        for( let child of this.children ) {
            const slide = child.querySelector( "content-slide" );
            if( slide ) {
                state.push(slide.getState())
            }
        }
        
        const json = JSON.stringify(state);
        window.location.hash = deflateToBase64( json );
    }

    newSection() {
        this.innerHTML += `<section>${this.template}</section>`;
        this.connectedCallback();
    }

    nextSlide() {
        let first = true;
        let lastneg = false
        for( let section of this.querySelectorAll("section" ) ) {
            let top = section.getBoundingClientRect().top
            if( top > 0 && lastneg ) {
                section.scrollIntoView()
                return;
            }
            lastneg = top <= 0
            if( first && top > 0 ) {
                lastneg = true;
                }
            first = false;
        }
    }

    prevSlide() {
        let last = undefined
        for( let section of this.querySelectorAll("section" ) ) {
            let top = section.getBoundingClientRect().top

            if( top >= 0 && last) {
                last.scrollIntoView();
                return;
                }
            last = section;
        }
    }

}

customElements.define("slides-holder", SlidesHolder);

document.addEventListener('keydown', function(event) {
    const body = document.querySelector( "body" );
    const slides = document.querySelector( "slides-holder ");

    if( event.target == body ) {
        if( event.key === 'ArrowRight' ) {
            slides.nextSlide();
        }
        
        if( event.key == 'ArrowLeft' ) {
            slides.prevSlide();
        }

        if( event.key == 'n' ) {
            slides.newSection();
        }
    }
});

class ContentSlide extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        for( let child of this.children ) {
            child.setAttribute( "contenteditable", true);
            // child.addEventListener( "click", (e) => {console.log( "click", e.target );} )
            child.addEventListener( "input", (e) => {
                document.querySelector("slides-holder").getState();
            } );
        }
    }

    getState() {
        let state = [];
        for( let child of this.children ) {
            state.push( {nodeName: child.nodeName, text: child.innerHTML})
        }

        return state;
    }
}

customElements.define("content-slide", ContentSlide);
