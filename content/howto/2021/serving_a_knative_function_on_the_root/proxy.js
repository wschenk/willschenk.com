const http = require('http');
const httpProxy = require('http-proxy');

const remote = process.env.REMOTE_TARGET || "http://homepage.default.svc.cluster.local";

console.log( "Proxy starting up on port 3000" );
console.log( `Proxing to ${remote}` );

httpProxy.createProxyServer({
    target: remote,
    changeOrigin: true
}).listen(3000);
