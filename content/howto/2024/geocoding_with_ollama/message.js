import ollama from 'ollama';
import { argv } from 'node:process';

const city = argv[2] ? argv[2] : 'bennington, vt'

const msgs = [
    { "role": "user", content:
      `where is the city ${city} in north america,
what is the city like, and where
is it located in latitude and longitude
in decimal` }
]

const output = await ollama.chat({ model: "Mistral:7b", messages: msgs })

console.log(output.message.content)
