const COOKIE_AUTH_TOKEN = 'tezlab_token';

function loadProfile( success, error ) {
    const profile = cacheGet( 'profile' );
    if( profile ) {
        success( profile );
        return;
    }
    
    if( cacheGet( COOKIE_AUTH_TOKEN ) ) {
        fetchGet( '/v1/profile' )
            .then( res => res.json() )
            .then( json => {
                if( json.success == false ) {
                    error( json );
                } else {
                    cacheSet( 'profile', json );
                    success( json );
                }})
            .catch( err => console.log( err ) );
    } else {
        error( {errors: 'Please log in'} );
    }
}

function login( email, password, success, error ) {
    fetchPost(`/v1/token`, JSON.stringify({ email, password }))
        .then(res => res.json())
        .then(json => {
            if( json.success == false ) {
                error( json )
            } else {
                cacheSet( COOKIE_AUTH_TOKEN, json.user.user_token );
                success( json );
            }})
        .catch(err => console.log( err ) );
}

function fetchPost( endpoint, body ) {
    const headers = new Headers();
    headers.append('Content-Type', 'application/json');
    const fetchInit = {
        method: 'POST',
        headers,
        body,
    };
    return fetch(endpoint, fetchInit);
};
      
function fetchGet(endpoint) {
    const headers = {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
    }
    const authToken = cacheGet( COOKIE_AUTH_TOKEN );
    if (authToken) {
        headers['Authorization'] = `Token token=${authToken}`;
    }
    const fetchInit = {
        method: 'GET',
        headers,
        credentials: 'include',
    }
    return fetch(endpoint, fetchInit);
}

function getCookie(cname) {
    let name = cname + "=";
    let decodedCookie = decodeURIComponent(document.cookie);
    let ca = decodedCookie.split(';');
    for(var i = 0; i <ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') {
            c = c.substring(1);
        }
        if (c.indexOf(name) == 0) {
            return c.substring(name.length, c.length);
        }
    }
    return undefined;
}

function cacheGet( key ) {
    const value = localStorage.getItem( key );
    if( value ) {
        return JSON.parse( value );
    }
    return undefined;
}

function cacheSet( key, value ) {
    localStorage.setItem( key, JSON.stringify( value ) );
}
