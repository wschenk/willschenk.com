import { serve } from "https://deno.land/std@0.51.0/http/server.ts";
import { redis } from "./redis.ts";

const PORT = Deno.env.get('PORT') || 8080;
const s = serve(`0.0.0.0:${PORT}`);

console.log(`Server started on port ${PORT}`);
for await (const req of s) {
    const cnt = await redis.incr( `counter:${req.url}` );
    const body = new TextEncoder().encode(`Hello, ${req.url} ${cnt} times!\n`);
    req.respond({ body });
}
