require 'ollama-ai'

client = Ollama.new(
  credentials: { address: 'http://localhost:11434' },
  options: { server_sent_events: true }
)

result = client.generate(
  { model: 'gemma:7b',
    prompt: 'Hi!' }
) do |event, raw|
  print event['response']
  $stdout.flush
end
puts
