require 'ollama-ai'

def get_response( message, context = nil )
  client = Ollama.new(
    credentials: { address: 'http://localhost:11434' },
    options: { server_sent_events: true }
  )
  
  result = client.generate(
    { model: 'zephyr', #gemma:7b',
      context: context,
      prompt: message }
  ) do |event, raw|
    print event['response']
    $stdout.flush
    context = event['context']
  end
  puts

  context
end

context = nil
message = ""
while message != "bye"
  print ">>> "
  $stdout.flush
  message = gets.chomp

  context = get_response( message, context )
end
