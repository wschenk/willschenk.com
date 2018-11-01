---
title: "Using Firebase to authenticate only to a specific domain"
subtitle: "Build internal only apps"
tags:
  - howto
  - firebase
  - google_apps
---

Sometimes its nice to throw something quickly up that only people inside your organization have access to.  Lets deploy a react app to firebase to show how this works.

## Get firebase up and running

Then lets create a sample app to contain our project.
```
$ npm install -g create-react-app firebase-tools
$ create-react-app forecaster
$ cd forecaster
```

Next lets install the firebase javascript SDK into our project.

```
$ npm install --save firebase
```


Now lets go to the [firebase console](https://console.firebase.google.com/u/0/) and create a new app.  Once it's created, go into the web settings and copy the config.js api keys from inside the `<script>` tags, and put them into `config/firebase.js` below.  We are importing firebase from the npm node_modules, setting up the config file, and then initializing the firebase app.  We are also exporting the `GoogleAuthProvider`:

```js
import firebase from 'firebase';

// Initialize Firebase
var config = {
  apiKey: "AIzaSyBgQsWGYtke6jbeOpHY8oXeeHIX8Vt39SE",
  authDomain: "honey-b6642.firebaseapp.com",
  databaseURL: "https://honey-b6642.firebaseio.com",
  projectId: "honey-b6642",
  storageBucket: "honey-b6642.appspot.com",
  messagingSenderId: "847930143737"
};

firebase.initializeApp(config);

export const google = new firebase.auth.GoogleAuthProvider();
export const auth = firebase.auth();
export default firebase;
```

Now we can login to firebase, and once that's working, we'll initialize our project

```
$ firebase login --reauth
$ firebase init
```

select database, functions, and hosting

Select a project

Make sure that you set it up as a single page apps


## Start writing code
