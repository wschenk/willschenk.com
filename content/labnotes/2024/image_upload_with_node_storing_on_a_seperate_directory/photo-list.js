const host = import.meta.env.MODE == 'development' ? "http://localhost:3000" : ""

class PhotoList extends HTMLElement {
    connectedCallback() {
        this.list = [];
        this.queryList();
        this.render();

        window.addEventListener( "refresh", () => this.queryList() );
    }

    async queryList()
    {
        const response = await fetch( `${host}/images` )
        const json = await response.json()
        this.list = json.entries
        this.render()
    }

    render() {
        let h = "<ul>"

        for( let i = this.list.length-1; i >= 0; i-- ) {
            let img = this.list[i];

            h += `<li><img src="${host}${img}" style="max-width: 300px"></li>`
        }

        h += `</ul>`

        this.innerHTML = h;
    }
}

customElements.define( 'photo-list', PhotoList );
