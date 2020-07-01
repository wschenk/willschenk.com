import { listenAndServe } from "https://deno.land/std/http/server.ts";
import {
    acceptWebSocket,
    acceptable,
    isWebSocketCloseEvent,
    isWebSocketPingEvent,
    WebSocket,
} from "https://deno.land/std/ws/mod.ts";

const portString = Deno.env.get('PORT') || "3000";
const port = parseInt( portString );

async function echo(sock: WebSocket) {
    console.log("socket connected!");
    try {
        for await (const ev of sock) {
            if (typeof ev === "string") {
                // text message
                console.log("ws:Text", ev);
                await sock.send(ev);
            } else if (ev instanceof Uint8Array) {
                // binary message
                console.log("ws:Binary", ev);
            } else if (isWebSocketPingEvent(ev)) {
                const [, body] = ev;
                // ping
                console.log("ws:Ping", body);
            } else if (isWebSocketCloseEvent(ev)) {
                // close
                const { code, reason } = ev;
                console.log("ws:Close", code, reason);
            }
        }
    } catch (err) {
        console.error(`failed to receive frame: ${err}`);

        if (!sock.isClosed) {
            await sock.close(1000).catch(console.error);
        }
    }

}

listenAndServe( `0.0.0.0:${port}`, async (req) => {
    if (acceptable(req)) {
        acceptWebSocket({
            conn: req.conn,
            bufReader: req.r,
            bufWriter: req.w,
            headers: req.headers,
        }).then(echo);
    } else {
        const decoder = new TextDecoder("utf-8");
        const bytes = Deno.readFileSync("echo.html");
        const body = decoder.decode(bytes);  
        req.respond({ body });
    }

});

console.log(`Server started on port ${port}`);
