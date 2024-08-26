customElements.define( 'network-status', class NetworkStatus extends HTMLElement {
    connectedCallback() {
        this.online = navigator.onLine;
        this.render();

        window.addEventListener( "online", () => {
            this.online = true;
            this.render();
        })

        window.addEventListener( "offline", () => {
            this.online = false;
            this.render();
        })
    }

    render() {
        let h = `<p>${this.online ? "Online" : "Offine"}</p>`

        if( navigator.connection ) {
            let c = navigator.connection;

            h += `<p>effectiveType: ${c.effectiveType}</p>`
            h += `<p>downlink: ${navigator.connection.downlink}mbs</p>`
            h += `<p>rtt: ${navigator.connection.rtt}ms</p>`
        }
        
        this.innerHTML = h;
    }
})
