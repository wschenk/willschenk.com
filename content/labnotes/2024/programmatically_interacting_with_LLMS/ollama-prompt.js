import ollama from 'ollama';

const msgs = [
    {
        "role": "system",
        "content": `You are a creative director at a
  marketing agency helping clients brain storm interesting ideas.`
    },
    {
        "role": "user",
        content: "Five cute names for a pet penguin"
    }
]

const output = await ollama.chat({ model: "mistral", messages: msgs })

console.log(output.message.content)
