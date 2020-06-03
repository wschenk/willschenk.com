const COOKIE_AUTH_TOKEN = 'tezlab_token';

function cacheGetFromEndpoint( key, endpoint, success, error ) {
    let data = JSON.parse( localStorage.getItem( key ) );
    if( data ) {
        success( data );
        return;
    }

    const headers = {
        'Accept': 'application/json',
        'Content-Type': 'application/json',
    }
    
    const authToken = JSON.parse( localStorage.getItem( COOKIE_AUTH_TOKEN ) );
    if (authToken) {
        headers['Authorization'] = `Token token=${authToken}`;
    }
    
    const fetchInit = {
        method: 'GET',
        headers,
        credentials: 'include'
    }
    fetch(endpoint, fetchInit)
        .then( res => res.json())
        .then( json => {
            if( json.success == false ) {
                error( json );
            } else {
                localStorage.setItem( key, JSON.stringify( json ) );
                success( json );
            }
        })
        .catch( err => {
            console.log( err );
            error( err );
        })
}

function loadProfile( success, error ) {
    cacheGetFromEndpoint( 'profile', '/v1/profile', success, error );
}

function login( email, password, success, error ) {
    const headers = new Headers();
    headers.append('Content-Type', 'application/json');
    const fetchInit = {
        method: 'POST',
        headers,
        body: JSON.stringify( {email, password} )
    };
    fetch(`/v1/token`, fetchInit )
        .then(res => res.json())
        .then(json => {
            if( json.success == false ) {
                error( json )
            } else {
                localStorage.setItem( COOKIE_AUTH_TOKEN, JSON.stringify( json.user.user_token ) );
                success( json );
            }})
        .catch(err => console.log( err ) );
}
