var NpmApi = require('npm-api');

(async () => {
    const repo = new NpmApi().repo( 'npm-api' );

    const pkg = await repo.package()

    console.log( "Name        ", pkg.name );
    console.log( "Version     ", pkg.version );
    console.log( "Description ", pkg.description );
    console.log( "License     ", pkg.license );
    console.log( "Homepage    ", pkg.homepage );
    console.log( "Repository  ", pkg.repository );
    console.log( "Clean repo  ", pkg.repository.url.replace( /git\+/g, '' ) );
    console.log( "Bug         ", pkg.bugs );
})()
