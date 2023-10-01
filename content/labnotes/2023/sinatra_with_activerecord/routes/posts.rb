# routes/posts.rb
require_relative '../models/post.rb'

get '/posts' do
  Post.all.to_json
end

post '/posts' do
  p = Post.new( name: params[:name], body: params[:body] )
  if !p.save
    p.errors.to_json
  else
    p.to_json
  end
end
