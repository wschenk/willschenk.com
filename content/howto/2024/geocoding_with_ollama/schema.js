import ollama from 'ollama';
import schema_message from './schema_message.js'
import { argv } from 'node:process';

const city = argv[2] ? argv[2] : 'bennington, vt'

const output = await ollama.chat(
    schema_message( city ) )

console.log(output.message.content)
