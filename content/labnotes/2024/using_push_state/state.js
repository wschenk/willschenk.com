function getState() {
    const queryString = window.location.search;
    const urlParams = new URLSearchParams(queryString);
    const entries = urlParams.entries();

    const state = {}
    for(const entry of entries) {
        console.log(`${entry[0]}: ${entry[1]}`);
        state[entry[0]] = entry[1];
    }

    return state;
}

function updateState( state, push = false ) {
    const url = new URL( window.location.href )

    for( const entry of Object.entries(state) ) {
        url.searchParams.set( entry[0], entry[1] );
    }

    if( push ) {
        window.history.pushState( {}, "", url )
    } else {
        window.history.replaceState( {}, "", url );
    }
}

function updateDom( state ) {
    // Look for all elements with the specified ID and if found, set the value
    for( const entry of Object.entries(state) ) {
        const i = document.querySelector( `#${entry[0]}` )
        if( i ) {
            i.value = entry[1]
        }
    }
}

window.addEventListener("popstate", (event) => {
    state = getState();
    updateDom( state );
} )

// Initial state loading
let state = getState()
updateDom( state );

// Update value of the state on keyup events
document.querySelectorAll( "input[type='text']" ).forEach((input) => {
    input.addEventListener( "keyup", (e) => {
        state[e.target.id] = e.target.value
        updateState(state);
    } )
});

// Make the submit change the history
document.querySelectorAll( "input[type='submit']" ).forEach((input) => {
    input.addEventListener( "click", (e) => {
        e.preventDefault();
        updateState(state,true);
    } )
});
