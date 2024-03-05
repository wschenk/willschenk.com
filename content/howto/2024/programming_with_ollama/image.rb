require 'ollama-ai'
require 'base64'

client = Ollama.new(
  credentials: { address: 'http://localhost:11434' },
  options: {
    server_sent_events: true,
    connection: { request: { timeout: 120, read_timeout: 120 } } }
)

client.generate(
  { model: 'llava',
    prompt: 'Please describe this image.',
    images: [Base64.strict_encode64(File.read('newspapers.jpg'))] }
) do |event, raw|
  print event['response']
end

puts
