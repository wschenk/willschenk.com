require 'roda'

class App < Roda
  plugin :static, ["/images", "/css", "/js"]
  plugin :render

  route do |r|
    r.root do
      view( :homepage )
    end
  end
end
