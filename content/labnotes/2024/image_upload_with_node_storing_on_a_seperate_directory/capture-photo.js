class CapturePhoto extends HTMLElement {
    connectedCallback() {
        this.innerHTML= `<input type="file" name="selectedPicture" id="selectedPicture" 
     accept="image/*" capture
      />`;
        const input = this.querySelector( "input" );
        input.addEventListener( "change", (e) => {
            this.uploadPhoto();
            console.log( input );
            console.log( e );
        });
    }

    async uploadPhoto() {
        const formData = new FormData();
        const input = this.querySelector( "input" );

        formData.append( "image", input.files[0] );
        input.value = ''
        const host = import.meta.env.MODE == 'development' ? "http://localhost:3000" : ""

        const response = await fetch(`${host}/upload`, {
            method: "POST",
            body: formData,
        });

        const event = new CustomEvent("refresh" )
        window.dispatchEvent( event );

        const result = await response.json();
        console.log( result );
    }
}

customElements.define( 'capture-photo', CapturePhoto );
