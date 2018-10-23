---
title: Authenticated routes using react-router
date: 2017-12-05T22:19:56.643Z
tags:
  - react
  - howto
---

1. `create-react-app route-test`
2. `cd route-test`
3. `yarn add react-router-dom`

And for `src/App.js`:

```jsx
import React, { Component } from 'react';
import {
    BrowserRouter as Router,
    Route,
    Link,
    Switch
} from 'react-router-dom';

const Nav = () => (
  <ul>
    <li><Link to='/'>Home</Link></li>
    <li><Link to='/about'>About</Link></li>
    <li><Link to='/session'>Session</Link></li>
    <li><Link to='/secret'>Secret</Link></li>
  </ul>
)

const Welcome = () => (
  <div>
    <h1>Welcome</h1>
    <p>This is some text</p>
  </div>
)

const About = () => (
  <div>
    <h1>About</h1>
    <p>Sure, stuff</p>
  </div>
)

const Secret = () => (
  <div>
    <h1>This is a secret</h1>
    <p>Sorry, not everyone can see this</p>
  </div>
)

const Session = ({login}) => (
  <div>
    <button onClick={login}>Click me</button>
  </div>
)

class App extends Component {
  state = {login: false}
  login = () => {this.setState( {login: true} ) }

  render() {
    return (
      <Router>
        <div>
          <Nav/>
          { this.state.login && <p>Logged in</p>}
          <Switch>
            <Route path="/secret" component={Secret}/>
            <Route path="/about" component={About}/>
            <Route path="/session" component={() => <Session login={this.login}/> } />
            <Route component={Welcome}/>
          </Switch>
        </div>
      </Router>
    );
  }
}

export default App;
```

* We are using [Stateless Functional Components](https://hackernoon.com/react-stateless-functional-components-nine-wins-you-might-have-overlooked-997b0d933dbc "") to model out our nav bar, and a few pages.
*
`Link` is what we use to create a link to something that we define in our router. This needs to be a child of a `Router`, so our `Nav` component must be a child of that.
*
`Switch` is where we are putting out routes to keep everything nice and tidy.
* We have a very simple login system here, where if you go to `/session` and push a button, you are considered logged in.

### Secret isn’t so secret

Lets create an authenticated component. The idea is that if the user is not authenticated, we will redirect to `/session` which will show whatever login form we have.

```jsx
import React, { Component } from 'react';
import { Route, Redirect } from 'react-router-dom'

class AuthenticatedRoute extends Component {
  render() {
    const {authed, component: Component, ...rest} = this.props;

    if( authed ) {
      return <Route {...rest} render={(props) => <Component {...props }/>}/>
    } else {
      return <Route {...rest} render={(props) => <Redirect to={{pathname: '/session', state: {from: props.location}}} />}/>
    }
  }
}

export default AuthenticatedRoute;
```

Now we just need to update the switch to use the new `AuthenticatedRoute`. We are passing in an `authed` property that the main `App` component is maintaining.

```jsx
<AuthenticatedRoute authed={this.state.login} path=”/secret” component={Secret}/>
```

Now we need to update the `Session` component to handle the login. In the case that we aren’t logged in, we show a button that calls the `login` method on the container component. This will change the login state and trigger a rerender of the components. Once that happens, `authed` will be true so we look inside of the `location` property to see if there was a protected route stored in the router history state. If so, we redirect back to that. Otherwise, we redirect back to home.

```jsx
const Session = ({authed,location,login}) => {
  if( authed ) {
    let l = location.location;
    let pathname = "/";
    if( l && l.state && l.state.from ) {
      pathname = l.state.from.pathname;
    }
    return <Redirect to={pathname}/>
  } else {
    return (
      <div>
        <button onClick={() => {login()}}>Click me</button>
      </div>
    )
  }
}
```

And finally, we need to wire all this up in the main routes.

```jsx
<Route path="/session" render={(location) => <Session location={location} login={this.login} authed={this.state.authed}/> }/> } />
```

Here we aren’t just passing in `component={Session}` but rather a function that gets wired up to a number of local callbacks. This is mainly so that the `login` function can call `setState` and then trigger the render. In a case where this would be something that came from a server, you could use the normal asynchronous methods and not need to force this directly.
