import ollama from 'ollama';

const output = await ollama.list();

console.log(JSON.stringify(output))
