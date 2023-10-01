# routes/account.rb

require_relative '../models/account.rb'

post '/signup' do
  account = Account.new(
    name: params[:name],
    password: params[:password],
    password_confirmation: params[:password_confirmation] || '')

  if account.save
    account.to_json
  else
    account.errors.to_json
  end
end

post '/login' do
  account = Account.find_by( name: params[:name])&.authenticate(params[:password])

  if account
    session[:account_id] = account.id
    puts "setting session #{session[:account_id]}"
  end

  { success: account }.to_json
end

get '/private' do
  auth_check do
    { message: "This is a secret" }.to_json
  end
end

def auth_check
  unless session[:account_id]
    return { access: :denied }.to_json
  else
    return yield
  end
end
