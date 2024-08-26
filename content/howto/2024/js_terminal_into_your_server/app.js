import express from 'express';
import { createServer } from 'node:http';
import { Server } from 'socket.io';
import cors from 'cors';
import pty from 'node-pty'

const app = express();
app.use(cors())
app.use(express.static('dist'))

const server = createServer(app);
const io = new Server(server, { cors: { origin: '*', } })

io.on('connection', (socket) => {
    console.log('a user connected');

    const proc = pty.spawn( "/bin/bash",[], {
        name: 'xterm-color',
        cols: 80,
        rows: 80,
        cwd: process.env.HOME,
        env: process.env
    });

    proc.onData( (data) => io.emit( 'response', data ) )
    proc.onExit( (data) => {
        console.log( "exit data", data )
        io.emit( 'response', 'Process ended' );
        //socket.close();
    } )
    
    
    socket.on('disconnect', () => {
        console.log( "user disconnected, killing process" )
        proc.kill();
    });

    socket.on( 'resize', (cols, rows ) => {
        console.log( "-Resizing to ", cols, rows );
        proc.resize(cols, rows);
    } );

    socket.on('keyup', (msg) => {
        console.log('message: ' + msg);
        proc.write( msg );
    });
});

server.listen(3000, () => {
  console.log('server running on port 3000');
});
