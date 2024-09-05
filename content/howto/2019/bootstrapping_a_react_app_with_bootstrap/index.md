---
title: Bootstrapping a react app
subtitle: with bootstrap and font awesome
date: "2019-04-07"
tags:
  - react
  - bootstrap
  - javascript
obsolete: true
aliases:
  - "/articles/2019/bootstrapping_a_react_app_with_bootstrap"
---

Here's a quick recipe for getting a blank react project with bootstrap up and running. We'll walk
though all of the steps that you'll need to get a basic bootstrap based framework up and running,
ready for theming and component implementation using redux.

<!--more-->

## Steps

Install the following packages.

### Install node/nvm

If you don't have it

```bash
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.34.0/install.sh | bash
source ~/.profile
nvm install 10
nvm global 10
```

### Install `yarn`

If you are into that

```bash
npm install -g yarn
```

### `create-react-app`

`npx` is a fun tool to keep your global npm registry clean for seldom used packages.

```bash
npx create-react-app sampleapp
cd sampleapp
```

### Install packages

- `redux`, `react-redux` we are going to setup a redux project
- `react-bootstrap`, `bootstrap`, `node-sass` This is bootstrap and theming components
- `@fortawesome/react-fontawesome`, `@fortawesome/fontawesome-svg-core`, `@fortawesome/free-solid-svg-icons` Standard icon sets
- `holderjs` utility to help mock out images

```bash
yarn add node-sass react-bootstrap bootstrap @fortawesome/react-fontawesome \
  @fortawesome/fontawesome-svg-core @fortawesome/free-solid-svg-icons \
  react-redux redux holderjs
```

## Changing default files.

We aren't going to use the following files

```bash
rm src/App.css src/index.css src/logo.svg
```

Lets make a simple bootstrap _theme_ in `src/index.scss`:

```scss
// Override default variables before the import
$body-bg: #ddd;

// Import Bootstrap and its default variables
@import "~bootstrap/scss/bootstrap.scss";
```

Then edit `src/index.js` to use that instead. (Including the full file.)

```jsx
import React from "react";
import ReactDOM from "react-dom";
import "./index.scss";
import App from "./App";
import * as serviceWorker from "./serviceWorker";

ReactDOM.render(<App />, document.getElementById("root"));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
```

Create `src/store.js` to initialize the `redux` store:

```jsx
import { createStore } from "redux";
import reducers from "./reducers";

export default createStore(reducers);
```

Create `src/reducers.js` to boostrap your reducers:

```jsx
import { combineReducers } from "redux";

const sample = (state = {}, action) => {
  switch (action.type) {
    case "PING":
      return { ...state, pong: true };
    default:
      return state;
  }
};
export default combineReducers({
  sample,
});
```

Now we can update `src/App.js` to tie it all together:

```jsx
import React, { Component } from "react";
import { Provider } from "react-redux";
import store from "./store";

class App extends Component {
  render() {
    return <Provider store={store}></Provider>;
  }
}

export default App;
```

At this point you should have a totally empty page that ready to start adding logic.

## Adding some Bootstrap

Create a sample navbar in `src/components/navbar.js`.
We are importing only the navbar component of `react-bootstrap`, only the parts of it that we need.
We also have an example of using `react-fontawesome` to bring in an icon.

```jsx
import React from "react";
import Navbar from "react-bootstrap/Navbar";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faAdjust } from "@fortawesome/free-solid-svg-icons";

const N = () => (
  <Navbar bg="light">
    <Navbar.Brand href="#home">
      <FontAwesomeIcon icon={faAdjust} /> Navbar with text
    </Navbar.Brand>
    <Navbar.Toggle />
    <Navbar.Collapse className="justify-content-end">
      <Navbar.Text>
        Signed in as: <a href="#login">Mark Otto</a>
      </Navbar.Text>
    </Navbar.Collapse>
  </Navbar>
);

export default N;
```

To see this work, we need to edit `src/App.js`:

```jsx
import React, { Component } from "react";
import { Provider } from "react-redux";
import store from "./store";
import Navbar from "./components/navbar";

class App extends Component {
  render() {
    return (
      <Provider store={store}>
        <Navbar />
      </Provider>
    );
  }
}

export default App;
```

## Lets make some pages

Here's a simple example of a jumbotron with, with three columns of images with captions
below. On mobile they will only show one column.

in `src/components/pages/home.js`:

```jsx
import React from "react";
import Jumbotron from "react-bootstrap/Jumbotron";
import Container from "react-bootstrap/Container";
import Button from "react-bootstrap/Button";
import Row from "react-bootstrap/Row";
import Col from "react-bootstrap/Col";
import Figure from "react-bootstrap/Figure";
import "holderjs";

const Header = () => (
  <Jumbotron fluid>
    <Container>
      <h1 className="display-4">Welcome to the app</h1>
      <p className="lead">
        Templating the boilerplate makes it easier to focus on what we are
        doing!
      </p>
      <hr className="my-4" />
      <Button variant="primary" size="lg" href="#about">
        About
      </Button>
    </Container>
  </Jumbotron>
);

const Feature = ({ description }) => (
  <Col md>
    <Figure>
      <Figure.Image
        width={400}
        height={300}
        alt="400x300"
        src="holder.js/400x300"
      />
      <Figure.Caption className="text-centered">{description}</Figure.Caption>
    </Figure>
  </Col>
);

const Home = () => (
  <>
    <Header />
    <Container>
      <Row>
        <Feature description="This first" />
        <Feature description="This second" />
        <Feature description="This third" />
      </Row>
    </Container>
  </>
);

export default Home;
```

To see this, in `src/App.js` import `src/pages/home.js` as `Home` and put the component
after the nav bar.

```jsx
<Provider store={store}>
  <Navbar />
  <Home />
</Provider>
```

## Multiple pages

And why not a simple about page at `src/pages/about.js`, since there's always
going to be some sort of about situation.

```jsx
import React from "react";

import Container from "react-bootstrap/Container";
import Media from "react-bootstrap/Media";
import "holderjs";

const About = () => (
  <Container>
    <h1 className="display-4">About this site</h1>
    <p>Here's a history of what has happened</p>
    <hr className="my-5" />
    <Media>
      <img
        width={64}
        height={64}
        className="mr-3"
        src="holder.js/64x64"
        alt="Generic placeholder"
      />
      <Media.Body>
        <h5>Media Heading</h5>
        <p>
          Cras sit amet nibh libero, in gravida nulla. Nulla vel metus
          scelerisque ante sollicitudin commodo. Cras purus odio, vestibulum in
          vulputate at, tempus viverra turpis. Fusce condimentum nunc ac nisi
          vulputate fringilla. Donec lacinia congue felis in faucibus.
        </p>
      </Media.Body>
    </Media>
  </Container>
);

export default About;
```

## React routing and actions

We're going to write out own router, since it's actually fairly simple and it
seems cleaner to keep everything inside of the redux store. We'll use hash based
routing for the time being as an example.

First, lets update our `src/reducers.js` to include storing the nav state. We're using
`window.location.hash` to set the initialState of the reducer.

```jsx
import { combineReducers } from "redux";

const nav = (state = { hash: window.location.hash }, action) => {
  switch (action.type) {
    case "HASH_CHANGED":
      return { ...state, hash: action.payload };
    default:
      return state;
  }
};

export default combineReducers({
  nav,
});
```

Lets create a `src/actions.js` file to actually change the hash when it happens.
I'm going to pull in the `store` directly here so that we have access to `dispatch`
and you can call this method directly from wherever you find yourself.

```jsx
import store from "./store";

export const hashChanged = (hash) =>
  store.dispatch({ type: "HASH_CHANGED", payload: hash });
```

Finally, let's create `src/pages/index.js` to pull all of this together.

1. We are creating a `Pages` component and registering an event listener when the url changes.
2. If it does, we call the `hashChanged` action to update the store.
3. We create a `ConnectedRouter` component which is passed in the `hash` from the store to our `Router`
4. Right now our `Router` is just looking for the `#about` hash specifically, but this is a place to extend in the future.
5. You can link to these pages using the normal href="#anchor" style of links. Nothing more magical than that.

```jsx
import React from "react";
import { connect } from "react-redux";
import Home from "./home";
import About from "./about";
import { hashChanged } from "../actions";

class Pages extends React.Component {
  componentDidMount() {
    // Listen for changes on the window object to update our store state
    this.popListener = window.addEventListener("popstate", () => {
      hashChanged(window.location.hash);
    });
  }

  render() {
    return <ConnectedRouter />;
  }
}

const Router = ({ hash }) => {
  if (hash === "#about") {
    return <About />;
  }

  return <Home />;
};

const mapStateToProps = (state) => ({
  hash: state.nav.hash,
});

const ConnectedRouter = connect(mapStateToProps)(Router);

export default Pages;
```

And then we need to change `src/App.js` to use this new component. Here's the full version:

```jsx
import React, { Component } from "react";
import { Provider } from "react-redux";
import store from "./store";
import Navbar from "./components/navbar";
import Pages from "./pages";

class App extends Component {
  render() {
    return (
      <Provider store={store}>
        <Navbar />
        <Pages />
      </Provider>
    );
  }
}

export default App;
```

## This router is simplistic

We don't do any parameter parsing, which is something that you could add. Additional this is set to use hash based routing, rather than the arguably more sexy HTML5 'pushState' and 'replaceState' routing that would make the url "cleaner". However I almost always deploy things off of a relative path, so that's not interesting to me. It may be interesting to you however.

Add `"homepage":"."` inside of your `package.json` to force the `create-react-app` build process to use relative paths.

## Note on actions

This is not the normal way that people use `actions` in a lot of the redux documentation that's out there. Normally the `actions.js` file contains _action creators_ which return a Object/hash containing at least an `action.type`. Then you need to get a hole of a `dispatch` method on the `store` object, normally by passing in a second argument into `connect` (`mapDispatchToPros`).

This seems unecessarily complicated to me. In our `action.js` which import the store directly, and when you call one of the exported functions it dispatches to the store directly. I'm not sure what I'm missing but it seems to work well.

## Wrap up

I hope this helps!
