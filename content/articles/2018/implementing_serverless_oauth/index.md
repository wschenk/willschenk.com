---
title: "Implementing Serverless OAuth"
subtitle: "for JAM Stacks and static sites"
date: 2018-11-01
draft: true
tags:
  - howto
  - static_sites
  - functions
  - firebase
---

Most of the serverless platforms have their own forms of authentication, but it might not support the specific service that you are looking to use.  Lets go through how we can build a react single page app, hosting on firebase, that talks to the unsplash service directly.  It will be hosted on firebase stoage, and with a tiny bit of firebase functions to tie it together.

## How oauth works

Here is the overall process:

<img src="oauth.sequence.svg" alt="sequence flow" class="img-fluid"/>

There are 3 entities -- the user's browser, our server, and the third party service.

1. On user action, such as pressing a Log in With Google button, a request is made to your server.
2. A signed request is generated which includes the access levels that we wish to be granted.  The client ID and client secret, which are provided by the service provider (under something like Create An App) are used to sign this request so that the server knows which app is requesting permission.
3. The browser is redirected to the third party service with this request.
4. The user interacts with the third party service, and grants us the permission.
5. The browser is redirected to the "call back url", which is generally needs to be white listed at the same place you got your client id and secrets.
6. The authcode is then used by our server to request an access_token from the service.  authcode are very short lived so prevent play back attacks since they are exposed to the browser.  Access tokens are longer lived and aren't generally exposed to the browser.
7. The server (optionally) gets information for the account, for example name and avatar photos, to configure a local account.
8. A logged in version of the site is returned to the user's browser.

In this scenario:

1. The user's password is never seen by our server.
2. The user doesn't need to remember yet another password for your site.
3. The access token is never seen by the user.
4. Access Tokens can be revoked by the third-party service at any time.
5. Access Tokens can be tied to a developer, so bad actors on the server level can be locked out.
6. The third party service can grant fine grained control of what data they grant access to.

## What do we need to implement

1. A client ID from the third party service.
2. A client secret from the third party service.
3. An auth method to contruct the initial requests.
4. An auth_callback method to process the final request.
5. An auth callback url that is granted access by the third party service's app configuration.

The first 2 are created at a third party service.  For this example, I'm going to choose [firebase](https://firebase.google.com/) to host our functions and website, and [Unsplash](https://unsplash.com/developers) since it's a pretty fun third-party service that's a bit off the beaten path and its not too hard to get an app registered.  

### Create the app on the third party service

Go ahead and follow that the unsplash link and register as a developer.

Then press the big `New Application` button and verify that you are going to follow the rules.  Give your application a name and a quick description, then create!

Note your Application Key and Secret.  This could aslo be called your client id and secret.  We'll need those later.  Set your callback url to `https://localhost:3000/auth_callback`.  Once we deploy to firebase we'll need to go back with our actual URL, but this is what we'll use fore testing.

### Create our javascript template

We are going to use `create-react-app` and then going to add firebase to it.

```bash
$ npx create-react-app unsplash-test
$ cd unsplash-test
```

### Setup firebase

Now we setup our "server" on the [FireBase console](https://console.firebase.google.com/u/0/)  Create a new application.  Select the web integration, and copy the example config into `src/firebaseConfig.js`, something like below:

```js
var config = {
  apiKey: "AIzY....",
  authDomain: "honey-b6642.firebaseapp.com",
  databaseURL: "https://honey-b6642.firebaseio.com",
  projectId: "honey-b6642",
  storageBucket: "honey-b6642.appspot.com",
  messagingSenderId: "847930143737"
};
```

If you haven't already, it's time to install the `firebase-tools` page.  I'm using yarn here:

```bash
$ yarn global add firebase-tools
```

And then lets add firebase to our project:

```bash
$ yarn add firebase
$ firebase login
$ firebase init
```

This will first prompt you to login to firebase so your machine has development credentials.  Then `init` will configure your local project with the correct configuration.

1. Select at least functions and hosting.
2. Choose the application that you just created.
3. I chose JavaScript instead of TypeScript.
4. Yes to ESLint.
5. Yes to install dependencies.
6. `build` as the hosting directory.
7. Yes to single page app.
8. No to overwriting `index.html` if asked.

Firebase should now be configured.  Lets do a quick test of the deploy.

```bash
$ yarn run build # Build the sample create-react-app project
$ firebase deploy # Push everything to firebase
```

And open your browser to the url that was `firebase deploy` spit out at the end.  If you see a blank screen, double check that in `firebase.json` you have `build` set as the `public` directory in hosting.

### Set the secrets for your firebase functions

We are going to store the oauth secrets inside of firebase to keep them seperate from your code. These are the application id and application secret that we got from unsplash above that you should have taken note off.  Lets set those in firebase now (and be sure you enter in your own secrets!)

```bash
$ firebase functions:config:set oauth.client_id=yourclientid
$ firebase functions:config:set oauth.client_secret=yourclientsecret
```

We can then pull these secrets back down locally into a firebase env file so that when we are testing out our firebase code it will behave like it will in production.


```bash
$ firebase functions:config:get > functions/.runtimeconfig.json
```

We don't want these files to go into git so

```
$ echo functions/.runtimeconfig.json >> .gitignore
```


### Setup firebase functions

Lets go into the `functions` directory and add a few npm modules.  One for the http, and the other for the oauth flow.

```bash
$ cd functions
$ yarn add express simple-oauth2 randomstring
```

Now lets create a simple app that we can use to test out our install.  Replace `functions/index.js` with:

```js
const functions = require('firebase-functions');
const express = require('express');

const oauth = functions.config().oauth;
const webApp = express()

webApp.get( '/auth', (req, res) => {

  res.send( "my client id is: " + oauth.client_id  )
})

exports.oauth = functions.https.onRequest( webApp )
```

Then run `firebase serve --only functions` to start up the api locally.  Be sure to check out the url that the proxy code is running on.  In my case, it's `http://localhost:5001/honey-b6642/us-central1/oauth`.  Once this is running you should be able to go to the url listed and see the client id from the configuration.

## Setup the react app

We need to point our react code to our firebase functions, and we are going to put that information into `env` files so that there is one place to swap them out later.  These need to be prefaced with `REACT_APP_` in order to play well with the `create-react-app` build process. Create a `.env.development` file with your information in it

```
REACT_APP_BASE_URL="http://localhost:5001/honey-b6642/us-central1/oauth"
```

We also don't want this file in source control:

```bash
$ echo .env.development >> .gitignore
```

Once we deploy the server to fire base, we can point this to our production instances.  Also, we will create another `.env.production` file for build time information.  Note that we don't want the oauth secrets here, since this is for the JavaScript code, not for the server functions.


First lets add some small styling to that our eyes won't hurt during development.


```bash
$ yarn add node-sass bootstrap reactstrap dotenv
```

Rename `src/index.css` to `src/index.scss`, and include the bootstrap sass files.  This is a bit overkill at the moment, but it will set things up for easy customization going forward.

```scss
@import "~bootstrap/scss/bootstrap.scss"
```

Be sure to update `src/index.js` to point to the correct style sheet, change `index.css` to `index.scss`

Now we can build out a scaffolding for `src/App.js`:

```jsx
import React, { Component } from 'react';
import { Jumbotron, Container } from 'reactstrap';


const LoginWindow = (props) => {
  return (
    <Jumbotron>
      <Container>
        <h1 className="display-3">Unsplash browser</h1>
        <p className="lead">This is an example of how to do something amazing</p>
        <p><a className="btn btn-primary" href={process.env.REACT_APP_BASE_URL + "/auth"}>Connect</a></p>
      </Container>
    </Jumbotron>
  )
}

class App extends Component {
  render() {
    return (
      <LoginWindow/>
    );
  }
}

export default App;
```

Now start up the server using `yarn start`, and press the `Connect` button.  You should now see the response from the firebase function running locally!



---

### References

1. https://firebase.google.com/docs/functions/get-started
2. https://firebase.google.com/docs/functions/local-emulator
2. https://www.netlify.com/blog/2018/07/30/how-to-setup-serverless-oauth-flows-with-netlify-functions--intercom/
3. https://github.com/Herohtar/netlify-cms-oauth-firebase
