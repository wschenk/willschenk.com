class App
  plugin :mailer_preview
  
  hash_branch "mail_view" do |r|
    r.is "welcome" do
      mail = Mailer.mail("/account/1/welcome")
      require 'pp'
      pp mail
      preview(mail)
    end
    
    r.is true do
      mailers = ["/welcome"]
      preview_index(mailers)
    end
  end
end
