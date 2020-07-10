import { listenAndServe, ServerRequest, Response } from "https://deno.land/std/http/server.ts";
import { posix } from "https://deno.land/std/path/mod.ts";
import {
    acceptWebSocket,
    acceptable,
    isWebSocketCloseEvent,
    isWebSocketPingEvent,
    WebSocket,
} from "https://deno.land/std/ws/mod.ts";
import { v4 } from 'https://deno.land/std/uuid/mod.ts'

const users = new Map<string, WebSocket>();

function broadcast(message: string): void {
  if (!message) return;
  for (const user of users.values()) {
    user.send(message);
  }
}

function broadcastPresence(): void {
    const message = JSON.stringify( {online: Array.from( users.keys() ) });
    broadcast( message );
}

async function sendMessage( from:string, to:string, message:string ) : Promise<boolean> {
    const dest = users.get( to );
    if( !dest ) return false;

    await dest.send( JSON.stringify( {from:from, payload: message} ) );

    return true;
}

async function messager(sock: WebSocket) {
    console.log("socket connected");

    const userId = v4.generate();

    console.log( `Assigned ${userId}` );
    await sock.send(JSON.stringify({id:userId}));
    users.set( userId, sock );

    broadcastPresence();

    try {
        for await (const ev of sock) {
            if (typeof ev === "string") {
                try {
                    const message = JSON.parse( ev );

                    console.log( message );

                    if( message.sendto ) {
                        const sent = await sendMessage( userId, message.sendto, message.payload );
                        await sock.send( JSON.stringify( { to: message.sendto, sent: sent } ) );
                    } else {
                        await sock.send( JSON.stringify( { to: message.sendto, sent: false } ) );
                    }
                } catch( e ) {
                    await sock.send( JSON.stringify( {badmessage: ev}));
                    console.log( e, ev );
                }

                //console.log("ws:Text", ev);
                //await sock.send(ev);
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

    users.delete( userId );

    broadcast( JSON.stringify( { left: userId } ) );
    broadcastPresence();
}

const port = Deno.env.get('PORT') || "3000";
const base = Deno.args[0];

export async function serveFile(
    req: ServerRequest,
    filePath: string
): Promise<Response> {
    const [file, fileInfo] = await Promise.all([
        Deno.open(filePath),
        Deno.stat(filePath),
    ]);
    const headers = new Headers();
    headers.set("content-length", fileInfo.size.toString());
//    const contentTypeValue = contentType(filePath);
//    if (contentTypeValue) {
//        headers.set("content-type", contentTypeValue);
//    }
    req.done.then(() => {
        file.close();
    });
    return {
        status: 200,
        body: file,
        headers,
    };
}

listenAndServe( `0.0.0.0:${port}`, async (req) => {
    if (acceptable(req)) {
        acceptWebSocket({
            conn: req.conn,
            bufReader: req.r,
            bufWriter: req.w,
            headers: req.headers,
        }).then(messager);
    } else {
        const path = req.url == '/' ? '/router.html' : req.url;
        let normalizedUrl = posix.normalize(path);
        try {
            normalizedUrl = decodeURIComponent(normalizedUrl);
        } catch (e) {
            if (!(e instanceof URIError)) {
                throw e;
            }
        }

        const fsPath = posix.join(Deno.cwd(), normalizedUrl);
        try {
            const fileInfo = await Deno.stat(fsPath);
            if (fileInfo.isDirectory) {
                req.respond( {body: `Can't serve a directory ${req.url}`} );
            } else {
                const response = await serveFile( req, fsPath );
                await req.respond(response);
            }
        } catch (e) {
            console.error(e.message);
            req.respond( {body: `Error: ${e.message}`, status: 500});
        } 
    }
});

console.log(`Server started on port ${port}`);
