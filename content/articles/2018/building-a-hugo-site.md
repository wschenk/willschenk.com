---
title: "Building a hugo site"
date: 2018-06-04
publishDate: 2018-06-04
tags:
  - "hugo"
  - "material-design"
  - "howto"
---

Lets go through the steps of creating a new hugo site and theme.  We will use webpack as our asset pipline, and material-design as our ui framework.  Lets get started!

```bash
$ hugo new site testsite
$ cd testsite
$ hugo new theme mytheme
```

Now lets `config.toml` to point to our new theme:

```toml
theme=”mytheme”
```

And start up hugo:

```bash
$ hugo server --watch -D &
```

This tells hugo to watch for changes and to build drafts.

## Setting up the asset pipeline

We are following directions from [the material design documentation](https://github.com/material-components/material-components-web/blob/master/docs/getting-started.md). Inside of the theme directory, lets create a basic `package.json` file:

```bash
$ cd themes/mytheme
```

`pacakge.json`:

```json
{
  "scripts": {
    "build": "webpack -p",
    "start": "APP_ENV=dev webpack --watch"
  }
}
```

Now install webpack and sass:

```bash
$ npm install --save-dev webpack@3 webpack-dev-server@2 css-loader sass-loader node-sass extract-loader file-loader autoprefixer postcss-loader postcss-easy-import
```

And create `webpack.config.js`

```javascript
const autoprefixer = require('autoprefixer');
const importer = require( 'postcss-easy-import' )

module.exports = [{
  entry: './theme.scss',
  output: {
    // This is necessary for webpack to compile
    // But we never use style-bundle.js
    filename: 'static/js/style-bundle.js',
  },
  module: {
    rules: [{
      test: /\.scss$/,
      use: [{
          loader: 'file-loader',
          options: {
            name: 'static/css/bundle.css',
          },
        },
        {
          loader: 'extract-loader'
        },
        {
          loader: 'css-loader',
          options: {
            includePaths: ['./node_modules']
          }
        },
        {
          loader: 'postcss-loader',
          options: {
            plugins: () => [autoprefixer({
              grid: false
            }),
          importer()]
          }
        },
        {
          loader: 'sass-loader',
          options: {
            includePaths: ['./node_modules']
          }
        },
      ]
    }]
  },
}];

```

This says to watch for `theme.scss`, run it through sass, and output the compiled file into `static/css/bundle.css`.  So lets create `themes/mytheme/theme.scss` now:

```sass
body {
  color: blue;
}
```

We will expand on that later.

Lets put a basic page in `themes/mytheme/layouts/_default/baseof.html`

```html
<html>
<head>
<title>This is my site</title>
<link href="/css/bundle.css" rel="stylesheet">
</head>

<body>
<h1>This is a title</h1>
<p>This is a paragraph</p>
</body>
</html>
```

And lets start up webpack!

```bash
$ npm run start
```

and visit http://localhost:1313 and you should see some nice blue text!  This is hugo rendering the html file we created, the `baseof.html`, which points to the css bundle that webpack created, which right now simply renders everything in blue.

## Adding some ES2015 goodness to the asset pipeline

Stop the webpack server using cntr-c, and install some additional development packages:

```bash
$ npm install --save-dev babel-core babel-loader babel-preset-es2015
```

Appened the following to `webpack.config.js`:

```javascript
module.exports.push({
  entry: './theme.js',
  output: {
    filename: 'static/js/bundle.js'
  },
  module: {
    loaders: [{
      test: /\.js$/,
      loader: 'babel-loader',
      query: {
        presets: ['es2015']
      }
    }]
  },
});
```

And then create `theme/mytheme/theme.js`:

```javascript
console.log( “Hello from theme.js” )
```

and replace `theme/mytheme/layouts/_default/baseof.html` to include a link to your new javscript bundle:

```html
<html>
<head>
<title>This is my site</title>
<link href="/css/bundle.css" rel="stylesheet">
</head>

<body>
<h1>This is a title</h1>
<p>This is a paragraph</p>
<script type="text/javascript" src="/js/bundle.js"></script>
</body>
</html>
```

Restart webpack


```bash
$ npm run start
```

and reload the page -- you should see your console log message!

## Lets start using Hugo

Lets start fleshing out the hugo templates.  Hugo has a couple of concepts to wrap your head around.  Single views, list views, blocks, and partials.  Additionally, you can define default views, lists views and single views by types.  We are going to work with the default types for now.

change `themes/mytheme/_defaults/baseof.html` to include a block:

```html
<html>
<head>
<title>This is my site</title>
<link href="/css/bundle.css" rel="stylesheet">
</head>

<body>
  {{ block "main" . }}
    <h1>This is the default content in the baseof.html template</h1>
  {{ end }}
  <script src="/js/bundle.js"></script>
</body>
</html>
```

`themes/mytheme/index.html`:

```html
{{ define "main" }}
This is the main block in index.html.
{{ end }}
```

Lets now build a simple single.html view:

`themes/mytheme/_default/single.html`:

```html
{{ define "main" }}
<div class="content">
  <h1>{{ .Title }}</h1>

  {{ .Content }}
</div>
{{ end }}
```

and a default list view in `themes/mytheme/_default/list.html`:

```html
{{ define "main" }}
  {{ $dateFormat := "Jan 21, 2014" }}
  {{ range .Data.Pages.ByDate }}
    <h2>
      <a href='{{ .Permalink | relURL }}' class="title-link">{{- .Title -}}</a>
      <time>{{ .Date.Format $dateFormat }}</time>
    </h2>
  {{ end }}
{{ end }}
```

lets create a new post and see what we have

```bash
$ hugo new posts/first-post.md
```

edit the file to clean up the date, so something like this:

```markdown
---
title: "First post"
date: 2018-06-04
tags:
  - "meta"
  - "example"
---

# This is my first post

We are here just testing to see how well this renders in our new templates.

Here's a list of stuff:

1. First thing
2. Second thing
3. Third thing

## Why things?

> Block quotes are super interesting, don't you think?

So fun!

```


Now go to http://localhost:1313/posts to see a list of your site posts!  Click on the title to view it in all of it’s glory!

## Adding tag views

`themes/mytheme/tags/list.html`:

```html
{{ define "main" }}

<div class="archives">
  {{ $dateFormat := "Jan 2014" }}
  {{ range .Data.Terms.ByCount }}
    <h1><a href="{{ "/tags/" | relLangURL }}{{ .Name | urlize }}">#{{ .Name }}</a> - ({{.Count}})</h1>
  {{ end }}
  {{ range .Data.Pages.ByDate.Reverse }}
    <h2>
      <time>{{ .Date.Format $dateFormat }}</time>
      <a href='{{ .Permalink | relURL }}' class="title-link">{{- .Title -}}</a>
    </h2>

    {{ if .Params.tags }}
      <div class="tagcontainer">
        <span class="spacer"></span>
        <ul>
          {{ range .Params.tags }}
            <li>
              <a href="{{ "/tags/" | relLangURL }}{{ . | urlize }}">#{{ . }}</a>
            </li>
          {{ end }}
        </ul>
      </div>
    {{ end }}
  {{ end }}
</div>
{{ end }}
```

Now if you go to http://localhost:1313/tags you should see a list of tags and the count of the posts that have those tags.  If you click on a tag itself you will see each of the posts that have those tags.  (Right now you only have one, post, but try creating another an seeing what happens!)

## Adding prismjs highlighting

```bash
$ npm install prismjs
```

In `themes/mytheme/theme.scss`:

```sass
@import "prismjs/themes/prism.css";
@import "prismjs/themes/prism-twilight.css";
@import "prismjs/plugins/toolbar/prism-toolbar.css";
```

And then in `themes/mytheme/theme.js` we are going to install prism, the scss language component, and the copy-to-clipboard plugin.

```javascript
import "prismjs/prism.js";
import "prismjs/components/prism-scss.js";
import "prismjs/components/prism-markdown.js";
import "prismjs/plugins/toolbar/prism-toolbar.js";
import "prismjs/plugins/copy-to-clipboard/prism-copy-to-clipboard.js";

```

## Adding material design

We're going to use the material design web components as our CSS framework.  I've written before about bootstrap, but I like Google's framework especially if you end up wanting it to look nice on mobile.  Lets install some components, and use the top-app-bar, typography, and drawer components.

```bash
$ cd themes/mytheme
$ npm install @material/theme @material/top-app-bar @material/typography @material/drawer normalize.css webfontloader
```

Update the main page to include some material design markup, here we are adding a default top nav bar which will go away when you scroll with a couple of action items in the header that link to different parts of the site.

`themes/mytheme/layouts/_default/baseof.html`

```html
<html>
<head>
<title>This is my site</title>
<link href="/css/bundle.css" rel="stylesheet">
</head>

<body class="mdc-typography">
  <header class="mdc-top-app-bar">
    <div class="mdc-top-app-bar__row">
      <section class="mdc-top-app-bar__section mdc-top-app-bar__section--align-start">
        <button class="material-icons mdc-top-app-bar__navigation-icon">menu</button>
        <span class="mdc-top-app-bar__title">{{ .Title }}</span>
      </section>

      <section class="mdc-top-app-bar__section mdc-top-app-bar__section--align-end">
        <ul class="bar__navigation">
          <li><a class="mdc-top-app-bar__navigation-item" href="/posts">Archives</a></li>
          <li><a class="mdc-top-app-bar__navigation-item" href="/about">About</a></li>
        </ul>
      </section>
    </div>
  </header>

  <div class="mdc-top-app-bar--fixed-adjust">
    {{ block "main" . }}
      <h1>This is the default content in the baseof.html template</h1>
    {{ end }}
  </div>
  <script src="/js/bundle.js"></script>
</body>
</html>

```

We then include the sass templates for the components that we are using in the `theme.scss` file.  We are adjusting the colors of the new `ul` elements that we added in the top bar.

```scss
@import "~normalize.css";
@import "prismjs/themes/prism.css";
@import "prismjs/themes/prism-twilight.css";
@import "prismjs/plugins/toolbar/prism-toolbar.css";

@import "@material/theme/_color-palette";

$mdc-theme-primary: $material-color-blue-800;
$mdc-theme-secondary: $material-color-pink-800;
$mdc-theme-on-primary: white;
$mdc-theme-on-secondary: white;
$mdc-theme-surface: $material-color-lime-50;

@import "@material/typography/mdc-typography";
@import "@material/top-app-bar/mdc-top-app-bar";
@import "@material/drawer/mdc-drawer";

ul.bar__navigation {
  @include mdc-typography(overline);
  list-style: none;

  li {
    display: inline;
    padding-left: 10px;

    a {
      color: $mdc-theme-on-primary;
      text-decoration: none;

      &:hover {
        text-decoration: underline;
      }
    }
  }
}
```

And finally, we need to load in the fonts (which we will get from google) and wire up the javascript for the topbar hiding.
in `theme.js` append:

```javascript
import WebFont from 'webfontloader';
WebFont.load({
  google: {
    families: ['Roboto', 'Material Icons']
  }
});

import {MDCTopAppBar} from '@material/top-app-bar/index';
const topAppBarElement = document.querySelector( '.mdc-top-app-bar' );
const topAppBar = new MDCTopAppBar(topAppBarElement);
```

## Adding fonts as assets

Lets explore adding a font installed from npm rather than using WebFont to load it.  We are going to use [Source Serif Pro](https://github.com/adobe-fonts/source-serif-pro) which has a nicer light version then is available on Google Fonts.

```bash
$ npm install git://github.com/adobe-fonts/source-serif-pro.git#release
```

In `webpack.config.js` add the following file loader to the main `theme.scss` loader, as an additional `rule`.

```javascript
    {
      exclude: [/\.(js|jsx|mjs)$/, /\.html$/, /\.json$/, /\.scss$/],
      loader: 'file-loader',
      options: {
        name: '[name].[hash:8].[ext]',
        outputPath: 'static/css/fonts',
        publicPath: 'fonts/'
      }
    }
```

Add a `fonts.scss` file

```scss
@font-face{
    font-family: 'Source Serif Pro';
    font-weight: 200;
    font-style: normal;
    font-stretch: normal;
    src: url('node_modules/source-serif-pro/EOT/SourceSerifPro-ExtraLight.eot') format('embedded-opentype'),
         url('node_modules/source-serif-pro/WOFF/OTF/SourceSerifPro-ExtraLight.otf.woff') format('woff'),
         url('node_modules/source-serif-pro/WOFF2/OTF/SourceSerifPro-ExtraLight.otf.woff2') format('woff2'),
         url('node_modules/source-serif-pro/OTF/SourceSerifPro-ExtraLight.otf') format('opentype'),
         url('node_modules/source-serif-pro/TTF/SourceSerifPro-ExtraLight.ttf') format('truetype');
}

@font-face{
    font-family: 'Source Serif Pro';
    font-weight: 300;
    font-style: normal;
    font-stretch: normal;
    src: url('node_modules/source-serif-pro/EOT/SourceSerifPro-Light.eot') format('embedded-opentype'),
         url('node_modules/source-serif-pro/WOFF/OTF/SourceSerifPro-Light.otf.woff') format('woff'),
         url('node_modules/source-serif-pro/WOFF2/OTF/SourceSerifPro-Light.otf.woff2') format('woff2'),
         url('node_modules/source-serif-pro/OTF/SourceSerifPro-Light.otf') format('opentype'),
         url('node_modules/source-serif-pro/TTF/SourceSerifPro-Light.ttf') format('truetype');
}

@font-face{
    font-family: 'Source Serif Pro';
    font-weight: 400;
    font-style: normal;
    font-stretch: normal;
    src: url('node_modules/source-serif-pro/EOT/SourceSerifPro-Regular.eot') format('embedded-opentype'),
         url('node_modules/source-serif-pro/WOFF/OTF/SourceSerifPro-Regular.otf.woff') format('woff'),
         url('node_modules/source-serif-pro/WOFF2/OTF/SourceSerifPro-Regular.otf.woff2') format('woff2'),
         url('node_modules/source-serif-pro/OTF/SourceSerifPro-Regular.otf') format('opentype'),
         url('node_modules/source-serif-pro/TTF/SourceSerifPro-Regular.ttf') format('truetype');
}

@font-face{
    font-family: 'Source Serif Pro';
    font-weight: 600;
    font-style: normal;
    font-stretch: normal;
    src: url('node_modules/source-serif-pro/EOT/SourceSerifPro-Semibold.eot') format('embedded-opentype'),
         url('node_modules/source-serif-pro/WOFF/OTF/SourceSerifPro-Semibold.otf.woff') format('woff'),
         url('node_modules/source-serif-pro/WOFF2/OTF/SourceSerifPro-Semibold.otf.woff2') format('woff2'),
         url('node_modules/source-serif-pro/OTF/SourceSerifPro-Semibold.otf') format('opentype'),
         url('node_modules/source-serif-pro/TTF/SourceSerifPro-Semibold.ttf') format('truetype');
}

@font-face{
    font-family: 'Source Serif Pro';
    font-weight: 700;
    font-style: normal;
    font-stretch: normal;
    src: url('node_modules/source-serif-pro/EOT/SourceSerifPro-Bold.eot') format('embedded-opentype'),
         url('node_modules/source-serif-pro/WOFF/OTF/SourceSerifPro-Bold.otf.woff') format('woff'),
         url('node_modules/source-serif-pro/WOFF2/OTF/SourceSerifPro-Bold.otf.woff2') format('woff2'),
         url('node_modules/source-serif-pro/OTF/SourceSerifPro-Bold.otf') format('opentype'),
         url('node_modules/source-serif-pro/TTF/SourceSerifPro-Bold.ttf') format('truetype');
}

@font-face{
    font-family: 'Source Serif Pro';
    font-weight: 900;
    font-style: normal;
    font-stretch: normal;
    src: url('node_modules/source-serif-pro/EOT/SourceSerifPro-Black.eot') format('embedded-opentype'),
         url('node_modules/source-serif-pro/WOFF/OTF/SourceSerifPro-Black.otf.woff') format('woff'),
         url('node_modules/source-serif-pro/WOFF2/OTF/SourceSerifPro-Black.otf.woff2') format('woff2'),
         url('node_modules/source-serif-pro/OTF/SourceSerifPro-Black.otf') format('opentype'),
         url('node_modules/source-serif-pro/TTF/SourceSerifPro-Black.ttf') format('truetype');
}
```

And now lets tell `theme.scss` to pull stuff in.  I'm including the top part of the file with all of the `@imports` so it's clear.

```scss
@import "~normalize.css";
@import "prismjs/themes/prism.css";
@import "prismjs/themes/prism-twilight.css";
@import "prismjs/plugins/toolbar/prism-toolbar.css";

@import "./fonts.scss";
@import "@material/theme/_color-palette";

$mdc-typography-font-family: Source Serif Pro, Roboto, serif;

$mdc-typography-styles-body1: (
  font-weight: 300
);

$mdc-theme-primary: $material-color-blue-800;
$mdc-theme-secondary: $material-color-pink-800;
$mdc-theme-on-primary: white;
$mdc-theme-on-secondary: white;
$mdc-theme-surface: $material-color-lime-50;

@import "@material/typography/mdc-typography";
@import "@material/top-app-bar/mdc-top-app-bar";
@import "@material/layout-grid/mdc-layout-grid";
```

And then lets add some basic styling for the content pages and the bottom of the same file:

```scss
.content {
  max-width: 800px;
  margin-left: auto;
  margin-right: auto;

  h1 {
    @include mdc-typography( headline2 );
  }

  h2 {
    @include mdc-typography( headline3 );
    font-family: Source Serif Pro, Roboto, serif;
  }

  p {
    @include mdc-typography(body1);
  }
}
```

## Adding next and back links
