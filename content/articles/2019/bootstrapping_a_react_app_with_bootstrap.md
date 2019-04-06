---
title: Bootstrapping a react app
subtitle: with bootstrap and font awesome
date: "2019-04-06"
draft: true
tags:
  - howto
  - react
  - bootstrap
  - javascript
---

Here's a quick receipt for getting a blank react project with bootstrap up and running.

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

```bash
yarn add node-sass react-bootstrap bootstrap @fortawesome/react-fontawesome \
  @fortawesome/fontawesome-svg-core @fortawesome/free-solid-svg-icons \
  react-redux redux
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
@import '~bootstrap/scss/bootstrap.scss';
```

Then edit `src/index.js` to use that instead.  (Including the full file.)

```jsx
import React from 'react';
import ReactDOM from 'react-dom';
import './index.scss';
import App from './App';
import * as serviceWorker from './serviceWorker';

ReactDOM.render(<App />, document.getElementById('root'));

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
```

Create `src/store.js` to initialize the `redux` store:

```jsx
import { createStore } from 'redux';
import reducers from './reducers';

export default createStore( reducers );
```

Create `src/reducers.js` to boostrap your reducers:

```jsx
import { combineReducers } from 'redux';

const sample = (state = {}, action) => {
  switch( action.type ) {
    case 'PING':
      return {...state, pong: true }
    default:
      return state;
  }
}
export default combineReducers({
  sample
})
```

Now we can update `src/App.js` to tie it all together:

```jsx
import React, { Component } from 'react';
import { Provider } from 'react-redux';
import store from './store';

class App extends Component {
  render() {
    return (
      <Provider store={store}>
      </Provider>
    );
  }
}

export default App;
```

At this point you should have a totally empty page that ready to start adding logic.


## Adding some Bootstrap

Create a sample navbar in `src/components/navbar.js`

```jsx
import React from 'react';
import Navbar from 'react-bootstrap/Navbar';
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faCoffee } from '@fortawesome/free-solid-svg-icons'

const N = () => (
  <Navbar bg='light'>
    <Navbar.Brand href="#home"><FontAwesomeIcon icon={faCoffee} /> Navbar with text</Navbar.Brand>
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





Now start the server

```
yarn start
```
