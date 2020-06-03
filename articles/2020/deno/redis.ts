import { connect, RedisConnectOptions } from "./deps.ts";

const REDIS_URL = Deno.env.get('REDIS_URL') || 'redis://127.0.0.1:6379';

// URL doesn't parse URIs https://github.com/denoland/deno/issues/5410
const redis_url = new URL( REDIS_URL.replace( /^redis:/, "http:" ) )

const redis_config : RedisConnectOptions = {
    hostname: redis_url.hostname,
    port: redis_url.port,
}

// Set a password if supplied
if( redis_url.password != "" ) {
    redis_config.password = redis_url.password;
}

console.log( `Connecting to redis at ${redis_config.hostname}:${redis_config.port}` )
export const redis = await connect( redis_config );

const count = await redis.incr( "counter" )

console.log( `Hello from Deno ${count}` )
