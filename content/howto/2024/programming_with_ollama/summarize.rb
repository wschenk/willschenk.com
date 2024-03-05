require 'ollama-ai'

def get_response( model,  message )
  client = Ollama.new(
    credentials: { address: 'http://localhost:11434' },
    options: { server_sent_events: true }
  )

  response = ""
  
  result = client.generate(
    { model: model,
      prompt: message }
  ) do |event, raw|
    response << event['response']
    print event['response']
    $stdout.flush
    context = event['context']
  end
  puts

  response
end

client = Ollama.new(
  credentials: { address: 'http://localhost:11434' },
  options: {
    server_sent_events: true,
    connection: { request: { timeout: 120, read_timeout: 120 } } }
)

models = client.tags.first['models'].collect { |x| x['name'] }

models.each do |model|
  puts "Running #{model}"
  get_response( model, "peter has 5 apples, and he gives 2 apples to susan.  \
                        now susan has 2 apples and peter has 3.  If john had \
                        7 apples and gives 3 to mary, how many apples does \
                          john have left?" )
  puts
end
