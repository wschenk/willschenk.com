require 'roda'

class App < Roda
  require_relative 'mailer.rb' # This comes later
  plugin :static, ["/images", "/css", "/js"]
  plugin :render
  plugin :hash_branches
  
  route do |r|
    r.root do
      view( :homepage )
    end
    
    Dir["routes/**/*.rb"].each do |route_file|
      require_relative route_file
    end
    
    r.hash_branches
  end
end
