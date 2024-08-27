require_relative '../models/account.rb'

class App
  hash_branch "account" do |r|
    r.is Integer do |id|
      #account = Account
      "account #{id}"
    end
    
    r.on 'hash' do |hash|
      r.get String do |hash|
        account = Account.where( login_hash: hash ).first
        if account
          "Hello #{account.email}"
        else
          "Not found"
        end
      end
    end
    
    r.is do
      p r.params
      
      r.get do
        "Post to create an account"
      end
              
      r.post do
        account = Account.new( name: r.params["name"], email: r.params["email"] )
        account.generate_login_hash
        
        if account.save
          "Hash is #{account.login_hash}"
        else
          puts "Missing something"
        end
      end
    end
  end
end
