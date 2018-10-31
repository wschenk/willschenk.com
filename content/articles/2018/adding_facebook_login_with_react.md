---
title: "Adding Facebook Login with react"
subtitle: "you can't escape it"
date: 2018-10-30
tags:
  - howto
  - facebook
  - react
---

Sometimes you just can't get away from facebook.  Here's a quick tutorial on how to add facebook login to your react app.

First you need to [create a facebook app](https://developers.facebook.com/docs/apps#register), which is an involved process especially if you want to let, you know, other people log in to your app.  Getting your app approved and otherwise up and running here is left as an excersize for you to figure out.

In the navigation menu:

1. click + Add Product
2. locate the Facebook Login product
3. click Set Up
4. Add web
5. Enter in the url for your site
6. Note your app id

## Create your react app

Then create a simple react app to test it out to make sure that the basics are wired up.

```bash
$ npx create-react-app facebook-login-test
$ cd facebook-login-test
```

Add the `react-facebook-login` npm package.

```bash
$ yarn add react-facebook-login
```

Now lets start up the server running under https, since Facebook will start rejecting api calls that arent from a secure site.

```bash
$ HTTPS=true yarn run start
```

We test it the facebook integration with some sample code in `App.js`.  Replace your `appId` with what you had before.

```jsx
import React from 'react';
import FacebookLoginWithButton from 'react-facebook-login';

const responseFacebook = (response) => {
  console.log(response);
}

const componentClicked = () => {
  console.log( "Clicked!" )
}

export default function App() {
  return (
    <FacebookLoginWithButton
      appId="1206715649505081"
      autoLoad
      fields="name,email,picture"
      onClick={componentClicked}
      callback={responseFacebook}
      icon="fa-facebook"/>
    )
}
```

Now if you click on the link you should be able to see that the `responseFacebook` function being called with the users information that was requested.  Hopefully you were able to see your account information! Since we have `autoLoad={true}` when you refresh the page `responseFacebook` will also be triggered on load since it detects that you already granted permission to your site.  Setting this to `false` will force the user to click again to initiate the process.

## A slightly more elaborate example

I've talked about using [Authenticated Routes Using React Router]({{< ref "authenticated_routes_using_react_router" >}}) and if you want a more thorough explication that would be a good place to start.  But quickly, lets just display the Facebook button if we don't know who you are, and then a simple card once we have your information.

```jsx
import React from 'react';
import FacebookLoginWithButton from 'react-facebook-login';

const componentClicked = () => {
  console.log( "Clicked!" )
}

const LoginButton = ({facebookResponse}) => (
  <FacebookLoginWithButton
    appId="1206715649505081"
    // autoLoad
    fields="name,email,picture"
    onClick={componentClicked}
    callback={facebookResponse}
    icon="fa-facebook"/>
  )


const UserScreen = ({user}) => (
  <>
    <h1>Welcome {user.name}!</h1>
    <p>{ user.email }</p>
    <img src={user.picture.data.url} height={user.picture.height} width={user.picture.width} alt="avatar"/>
  </>
)

class App extends React.Component {
  state = {user:false}
  facebookResponse = (response) => { console.log( response ); this.setState( {...this.state, user: response } ) }

  render() {
    return (
      <div style={{ margin: "auto", textAlign: "center", paddingTop: "2em" }}>
        { this.state.user ? <UserScreen user={this.state.user}/> :
          <LoginButton facebookResponse={this.facebookResponse}/>
        }
      </div>
    )
  }
}

export default App
```

---

References:

1. https://developers.facebook.com/docs/apps#register - Facebook documentation on registering an application
2. https://github.com/keppelen/react-facebook-login - The react plugin
