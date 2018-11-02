---
title: "Implementing Servless OAuth"
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



---

### References

1. https://www.netlify.com/blog/2018/07/30/how-to-setup-serverless-oauth-flows-with-netlify-functions--intercom/
2. https://github.com/Herohtar/netlify-cms-oauth-firebase
