require 'roda'

class App < Roda
  route do |r|
    r.root do
      "Hello there"
    end
  end
end
