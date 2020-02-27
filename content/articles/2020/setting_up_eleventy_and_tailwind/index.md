---
title: Settingup tailwind and eleventy
subtitle: An excersize in minimilism
tags:
  - howto
  - tailwind
  - eleventy
date: "2020-01-30"
draft: true
---

We are going to setup a project that lets us build static HTML sites with [eleventy](https://www.11ty.dev/), [tailwindcss](https://tailwindcss.com/), and [PurgeCSS](https://purgecss.com/). Our goals with this project are to make it easy to throw up good looking sites that are easy to come back to and update since the tooling is simple and gets out of the way.


| Tool | Rationale |
| ---- | --------- |
| eleventy | Its conceptually simple, lets you mix and match templating languages |
| tailwindcss | Style by adding CSS utility classes, so you're design and build process can all be while you are editing HTML not HTML + CSS which makes things easier to reason about |
| PurgeCSS | tailwindcss files are really big |

## Create the project

1. `mkdir projectname && cd projectname`
2. `npm init -y`
3. `npm install --save-dev @11ty/eleventy tailwindcss postcss-cli autoprefixer`

## Folder structure

1. `src/` where we write our code
2. `src/css` -> `build/css`
3. `src/images` -> `src/images`
4. `src/javascript` -> `src/javascript`
5. `src/layouts` our layout templates
6. `build/` where we generate things

## eleventy setup

[`.eleventy.js`](./.eleventy.js)

TODO Include embed

Now we can add some `script` tasks inside of `package.json` to run, watch, server, and build the site so we can see what's happening.

```js
  "scripts": {
    "build:eleventy": "eleventy",
    "serve:eleventy": "eleventy --serve",
    "debug:eleventy": "DEBUG=* eleventy"
  },
```

And create a simple file

{{% embed "src/index.html" "html" "yes" %}}


Now we can run `npm run serve:eleventy` to build and watch the site as you edit it. Visit `http://localhost:8080` to make sure that the server is running.


## Tailwind setup

Create the `tailwind.config.js`:

1. `npx tailwind init`


Setup the project and install the dependacies.

```bash
npm init -y
npm add tailwindcss postcss-cli autoprefixer 
npx tailwind init
```

In package.json
```json
  "scripts": {
    "build": "postcss tailwind.css -o public/css/tailwind.css"
  },
```

In postcss.config.js

```javascript
module.exports = {
  plugins: [
    require('tailwindcss'),
    require('autoprefixer'),
  ]
}
```

Then create tailwind.css:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

Then run

`npm run build` to create `public/css/tailwind.css`

Then create public/index.html

```html
 
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="X-UA-Compatible" content="ie=edge">
  <title>Document</title>
  <link rel="stylesheet" href="/css/tailwind.css">
</head>
<body>
  <h1 class="text-4xl font-bold text-center text-blue-500">Hello world!</h1>
</body>
</html>
```

Then install live-server if you don't have it

```bash
npm install -g live-server
```
and

```bash
live-server public
```

And you should see the html file in the browser.


## References

1. https://tailwindcss.com/
1. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template
1. https://developers.google.com/gmail/api/quickstart/js
1. https://github.com/danfascia/tai11s
1. https://github.com/tailwindcss/discuss/issues/243
1. https://api.slack.com/methods/users.list
