---
title: Setting up Devise with Twitter and Facebook and other Omniauth schemes without
  email addresses
subtitle: Connect connect connect
tags: howto, rails, oauth, happy_seed, ruby
date: 2015-01-16
---
Adding social login to your sites really makes it easier to get users onboard.  Devise is great to help get an authentication system up and running, but there are a few tricky things to get right.  The first challenge is that you don't always get the user's email address when the first connect.  The second challenge is that we want to request the minimum permissions first so that the user is more likely to sign up, and gradually ask more as the time arises.

This post is going to go through the strategy that [happy_seed](http://seed.happyfuncorp.com) uses to support these use cases.  The easiest way to get started is to use seed to get things up and running, but we'll walk through how to do it all in detail below.

## Install devise and omniauth
Let's install a few gems.  We'll go through how to install twitter, facebook, and google.

`Gemfile`:

```rb
gem 'devise', '~> 3.4'
gem 'omniauth'
gem 'omniauth-twitter'
gem 'omniauth-facebook'
gem 'omniauth-instagram'
gem 'twitter'
gem 'instagram'
gem 'omniauth-google-oauth2'
gem 'google-api-client', require: 'google/api_client'
```

Now we need to run the `devise` generators.  I like to copy over the views so that I can fix them up to look like the rest of my app.  If you are using seed, these views will be generated with `HAML` and the `bootstrap helpers` gem.

First install devise:

```sh
$ rails generate devise:install
```

`devise:install` copies over `config/initializers/devise.rb` and a localized message file.  We will configure devise here.  _Follow the outputed instuctions to setup flashes and mailer configs._

Now create a model, call it user:

```sh
$ rails generate devise User
```

This creates a `User` model and configures devise routes to use it.  We will edit both of these.

Finally, copy over the views so you can style them as you need:

```sh
$ rails generate devise:views
```

## Make sure regular login in works

Lets create a basic controller to see if our login works:

```sh
$ rails g controller welcome index
$ rake db:migrate
$ rails s
```

Edit `routes.rb`:

```rb
  get 'welcome/index'
  root 'welcome#index'
```

And change your `WeclomeController` to require user authentication:

```rb
class WelcomeController
  before_action :authenticate_user!

  def index
  end
end
```

Now if you go to [http://localhost:3000/](http://localhost:3000/) you should get redirected to a login page.  If you create a user you should be able to then see the protected page.

Let's add a logout button to the index page for testing: `app/views/welcome/index.html.erb`:

```erb
<%= link_to "Signout", destroy_user_session_path, method: :delete %>
```

Now we can go back and forth.  The `method: :delete` part is something that I often forget about.


## Configure Omniauth

We need to tell devise and omniauth how to talk to the various outside services.  The first thing you'll need to do is configure those services and collect their app ids and app secrets.  Then you put that information inside of `config/initializers/devise.rb`:

```rb
    config.omniauth :google_oauth2, ENV['GOOGLE_OAUTH2_APP_ID'], ENV['GOOGLE_OAUTH2_APP_SECRET'], scope: "email,profile,offline", prompt: "consent"
    config.omniauth :instagram, ENV['INSTAGRAM_APP_ID'], ENV['INSTAGRAM_APP_SECRET']
    config.omniauth :facebook, ENV['FACEBOOK_APP_ID'], ENV['FACEBOOK_APP_SECRET'], scope: "email"
    config.omniauth :twitter, ENV['TWITTER_APP_ID'], ENV['TWITTER_APP_SECRET']
```

Since we are building nice [12 Factor Apps](http://12factor.net) we pull the config from the environment.  seed uses the [dotenv gem](https://github.com/bkeepers/dotenv) to keep track of these things in a `.env` file, and when you deploy to heroku you will use heroku config variables.

We also pass in `scope`s to a few strategies, which is where we can configure omniauth to request specific permissions.  Sometimes you need to enable them on the remote side before you can request things (_e.g._ google, twitter) so make sure that things are setup there.

We'll go into how to dynamically set that scope later on.

## Tell Devise about omniauthable

Open up `app/models/user.rb` and add `:omniauthable` to your `devise` line and remove `:validatable`:

```rb
  devise :omniauthable, :database_authenticatable, :registerable,
         :recoverable, :rememberable, :trackable
```

Now you should see that there are a list of connect with our services when you go to your sign in or login pages.

## Create a FormUser to handle validations

Not all services return email addresses, and by default the devise validations expect them.  Let's move those validations out of the base `User` class into a `FormUser` class.

1. Remove `:validatable` from `app/models/user.rb` (which you've done above)
2. Tell devise to use our new model.
3. Create the forms_user.rb class.

Inside of `config/routes.rb`:

```rb
devise_for :users, class_name: 'FormUser'
```

And `app/models/form_user.rb` should look like:

```rb
class FormUser < User
  attr_accessor :current_password
  
  validates_presence_of   :email, if: :email_required?
  validates_uniqueness_of :email, allow_blank: true, if: :email_changed?
  validates_format_of     :email, with: Devise.email_regexp, allow_blank: true, if: :email_changed?

  validates_presence_of     :password, if: :password_required?
  validates_confirmation_of :password, if: :password_required?
  validates_length_of       :password, within: Devise.password_length, allow_blank: true

  def password_required?
    return false if email.blank?
    !persisted? || !password.nil? || !password_confirmation.nil?
  end

  def email_required?
    true
  end
end
```

The `class_name` inside of the devise config will tell it to use this class for building forms, and we have the validations on this class so our error messages will work on the site, but we'll be able to save objects without it.

## Create Identity model to store access_keys and metadata

Now we are ready to plug in oauth authentications.  The flow is:

1. User requests `/users/auth/:provider`, where provider one of the strategies that you defined above.
2. Omniauth does magic and directs the user to the remote service.
3. The user grants us access and is redirected to the callback path.
4. The OmniauthCallbacks controller is called on our application with the relavent info.

We will use this info to create the user.  We are also going to store it to be able to access the service on behalf of the user, and we'll need to store the `access_token` in order to do so.

Google is slightly more complicated and we'll need to store a `refresh_token` as well.

Lets create that model now:

```sh
$ rails model identity user:references provider:string accesstoken:string refreshtoken:string uid:string name:string email:string nickname:string image:string phone:string urls:string
```

And flesh out `app/models/identity.rb`

```rb
class Identity < ActiveRecord::Base
  belongs_to :user
  validates_presence_of :uid, :provider
  validates_uniqueness_of :uid, :scope => :provider

  def self.find_for_oauth(auth)
    identity = find_by(provider: auth.provider, uid: auth.uid)
    identity = create(uid: auth.uid, provider: auth.provider) if identity.nil?
    identity.accesstoken = auth.credentials.token
    identity.refreshtoken = auth.credentials.refresh_token
    identity.name = auth.info.name
    identity.email = auth.info.email
    identity.nickname = auth.info.nickname
    identity.image = auth.info.image
    identity.phone = auth.info.phone
    identity.urls = (auth.info.urls || "").to_json
    identity.save
    identity
  end
end
```

Now we need to tell devise to use this model.

## Create OmniauthCallbacksController to pull in data

We're going to build one method to handle the different authentication callbacks, called `generic_callback`.  The logic of this controller is:

1. Find or create an `Identity` object for the incoming oauth data.  Update it with the latest info.
2. If there is no user associated with the Identity, associate it with the current_user.
3. If there is no current_user, create a new User object.
4. If the User object doesn't have an email address set yet, but we do have one from the remote service, set the email address to that.
5. Log the user in and let the continue on their way.


First we need to tell devise about our controller in `routes.rb`

```rb
devise_for :users, class_name: 'FormUser', :controllers => { omniauth_callbacks: 'omniauth_callbacks' }
```

Create `app/controllers/omniauth_callback_controller.rb`:

```rb
class OmniauthCallbacksController < Devise::OmniauthCallbacksController
  def instagram
    generic_callback( 'instagram' )
  end

  def facebook
    generic_callback( 'facebook' )
  end

  def twitter
    generic_callback( 'twitter' )
  end

  def google_oauth2
    generic_callback( 'google_oauth2' )
  end

  def generic_callback( provider )
    @identity = Identity.find_for_oauth env["omniauth.auth"]

    @user = @identity.user || current_user
    if @user.nil?
      @user = User.create( email: @identity.email || "" )
      @identity.update_attribute( :user_id, @user.id )
    end

    if @user.email.blank? && @identity.email
      @user.update_attribute( :email, @identity.email)
    end

    if @user.persisted?
      @identity.update_attribute( :user_id, @user.id )
      # This is because we've created the user manually, and Device expects a
      # FormUser class (with the validations)
      @user = FormUser.find @user.id
      sign_in_and_redirect @user, event: :authentication
      set_flash_message(:notice, :success, kind: provider.capitalize) if is_navigational_format?
    else
      session["devise.#{provider}_data"] = env["omniauth.auth"]
      redirect_to new_user_registration_url
    end
  end
end
```

## Override RegistrationsController to handle adding email address and password

We want to let the user add an email address if they haven't already, and also let them set a password if they haven't already set one.  (Otherwise it requires the user to enter in `current_password`.)  Lets first tell devise about our new controller:

```rb
devise_for :users, class_name: 'FormUser', :controllers => { omniauth_callbacks: 'omniauth_callbacks', registrations: 'registrations'}
```

And then create that controller:

```rb
class RegistrationsController < Devise::RegistrationsController
  def update_resource(resource, params)
    if resource.encrypted_password.blank? # || params[:password].blank?
      resource.email = params[:email] if params[:email]
      if !params[:password].blank? && params[:password] == params[:password_confirmation]
        logger.info "Updating password"
        resource.password = params[:password]
        resource.save
      end
      if resource.valid?
        resource.update_without_password(params)
      end
    else
      resource.update_with_password(params)
    end
  end
end
```

And now we should be good!  Give it a go and see how it looks!

## Adding methods to User to get to the clients

This goes into `app/models/users.rb`:

```rb
  has_many :identities
  
  def twitter
    identities.where( :provider => "twitter" ).first
  end

  def twitter_client
    @twitter_client ||= Twitter.client( access_token: twitter.accesstoken )
  end

  def facebook
    identities.where( :provider => "facebook" ).first
  end

  def facebook_client
    @facebook_client ||= Facebook.client( access_token: facebook.accesstoken )
  end

  def instagram
    identities.where( :provider => "instagram" ).first
  end

  def instagram_client
    @instagram_client ||= Instagram.client( access_token: instagram.accesstoken )
  end

  def google_oauth2
    identities.where( :provider => "google_oauth2" ).first
  end

  def google_oauth2_client
    if !@google_oauth2_client
      @google_oauth2_client = Google::APIClient.new(:application_name => 'HappySeed App', :application_version => "1.0.0" )
      @google_oauth2_client.authorization.update_token!({:access_token => google_oauth2.accesstoken, :refresh_token => google_oauth2.refreshtoken})
    end
    @google_oauth2_client
  end
```

Now you can access a configured API client using things like `current_user.twitter`.

## Passing dynamic scopes to omniauth

In the current scheme above, you have to hard code the scopes that you want to request for the user which doesn't always work.  It would be better to only request write access if the user really needs to have it, and by default only get read-only access.  In order to do this we can leverage the `Omniauth` setup property.  Inside of `devise.rb` add `setup: true` to each of the services you want to be able to upgrade.

```rb
    config.omniauth :google_oauth2, ENV['GOOGLE_OAUTH2_APP_ID'], ENV['GOOGLE_OAUTH2_APP_SECRET'], scope: "email,profile,offline", prompt: "consent", setup: true
    config.omniauth :instagram, ENV['INSTAGRAM_APP_ID'], ENV['INSTAGRAM_APP_SECRET'], setup: true
    config.omniauth :facebook, ENV['FACEBOOK_APP_ID'], ENV['FACEBOOK_APP_SECRET'], scope: "email", setup: true
    config.omniauth :twitter, ENV['TWITTER_APP_ID'], ENV['TWITTER_APP_SECRET'], setup: true
```

Let's add a few routes in `routes.rb` that we can have the user link to:

```rb
  devise_scope :user do
    get '/users/auth/:provider/upgrade' => 'omniauth_callbacks#upgrade', as: :user_omniauth_upgrade
    get '/users/auth/:provider/setup', :to => 'omniauth_callbacks#setup'
  end
```

The first route is something that we'll have the user link to, using `user_omniauth_upgrade_path( :google_oauth2 )` for example.  The second `setup` route is what omniauth will call internally that we can use to change the `scope` parameter.  These go into `omniauth_callbacks_controller.rb`.

The first `upgrade` method looks at the provider and sets a `flash` variable for the additional access.  In this case, we are asking for the `https://www.googleapis.com/auth/admin.directory.user` also.

```
  def upgrade
    scope = nil
    if params[:provider] == "google_oauth2"
      scope = "email,profile,offline,https://www.googleapis.com/auth/admin.directory.user"
    end

    redirect_to user_omniauth_authorize_path( params[:provider] ), flash: { scope: scope }
  end
```

Then it directs the user back to the normal flow.

When you specify `setup: true` inside of the omniauth configuration, there is magic that calls the `setup_path` by default, and this is the method where we change the scope from the default in the strategy:

```rb
  def setup
    request.env['omniauth.strategy'].options['scope'] = flash[:scope] || request.env['omniauth.strategy'].options['scope']
    render :text => "Setup complete.", :status => 404
  end
```

Now in your views, you can do

```erb
<%= link_to "Upgrade Access", user_omniauth_upgrade_path( :google_oauth2 ) %>
```

And the user will go through the oauth flow again requesting the additional access.

## Conclusion

Making all of this work is possible, but there are a lot of fiddly little bits to make it work.  Both devise and all of the many omniauth strategies out there make it easy to add this functionality to your application.

Check out [seed](http://seed.happyfuncorp.com) to quickly create an application will all of this stuff done for you!