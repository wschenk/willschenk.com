import Alpine from './alpine.js'

Alpine.data('loader', (url) => ({
    url: url,
    error: null,
    data: null,
    init() {
        fetch( url )
            .then( (response) => {
                if( !response.ok ) {
                    return Promise.reject(response.statusText);
                } else {
                    return response.json()
                }
            })
            .then( (json) => {
                if( Array.isArray( json ) ) {
                    this.data = json
                } else {
                    this.data = new Array(json)
                }
            } )
            .catch( (error) => {
                this.error = error;
            })
    }
}) );
