import "@xterm/xterm/css/xterm.css"
import {Terminal} from "@xterm/xterm"
import { FitAddon } from '@xterm/addon-fit';
import { io } from "socket.io-client"

window.addEventListener( "load", () => {
    const terminal = document.getElementById( 'terminal' );

    const term = new Terminal();
    const fitAddon = new FitAddon();
    term.loadAddon(fitAddon);
    term.open(terminal);

    fitAddon.fit();

    window.term= term;

    const host = import.meta.env.MODE == 'development' ? "http://localhost:3000/" : undefined
    term.write( `Connecting to ${host}...\n\r` )
    const socket = io(host);

    socket.on( 'connect', (msg) => {
        term.write( `Connected.\n\r` );
        socket.emit( 'resize', term.cols, term.rows );
    } )

    socket.on( 'response', (msg) => {
        term.write( msg )
    } );

    term.onKey(e => {
        //console.log(e);
        e.domEvent.stopPropagation()
        e.domEvent.preventDefault()
        socket.emit( 'keyup', e.key );
    })

    /*
      term.onData( (d) => {
        console.log( "paste", d )
        socket.emit( 'keyup', d );
        })
        */

    window.addEventListener( 'resize', () => fitAddon.fit() );
    term.onResize( (sz) => {
        console.log( "resizing to", sz.cols, sz.rows );
        socket.emit( 'resize', sz.cols, sz.rows );
    })
} )
