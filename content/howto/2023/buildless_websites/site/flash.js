import Alpine from './alpine.js'

export function setFlash( message ) {
  window.localStorage.setItem( "flash", message )
}

export function getFlash(  ) {
    const msg = window.localStorage.getItem( "flash" )
    window.localStorage.removeItem("flash")
    return msg;
}

Alpine.data('flash', (url) => ({
    message: null,

    init() {
        this.message = getFlash();
        },

    setMessage(message) {
        setFlash( message )
    }
}))
