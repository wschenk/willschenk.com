class App::Mailer < Roda
   plugin :render, views: 'views/mail', layout: nil
   plugin :mailer
   
   route do |r|
     r.on "account", Integer do |id|
       puts "Looking up #{id}"
       @account = Account[id]
       no_mail! unless @account
       
       puts @account
       
       from "tasks@example.com"
       to @account.email
       r.mail "welcome" do
         subject "Your login hash"
         render('welcome_html')
         # text_part render( 'welcome_text' )
         #  html_part render( 'welcome_html' )
       end
     end
   end     
 end
