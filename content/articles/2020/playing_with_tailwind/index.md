---
title: Playing with tailwind
subtitle: An excersize in minimilism
tags:
  - howto
  - tailwind
date: "2020-01-30"
draft: true
---

We are going to use tailwindcss to build up a site.  We will start with a blank folder and a static HTML file to work on the designs, and then slowly bring in other tools when needed to add in functionality.

The site that we are going to build is a company directory that pulls in the data from Google Apps Suite.  The goal here is to build the simpleist thing possible and not get lost in the tooling.  We will start with a static site and explore different ways to pull in the data and style it.

## Create the project folder and base HTML file

```bash
mkdir projectname && cd projectname
```

Now we will create a base html file that we'll use to start styling. We are going to include the full `tailwind.min.css` file from a CDN so there's no build process here.

{{% embed "static/index.html" "html" %}}

Lets startup a preview server by running `npx live-server` which should open up a browser window which will update files.  You should see a window open up.  If you edit some of the text, you should see it updated in the browser once you save. So far so good, all we've needed is a html file and node installed on the machine.

## The profile page

I pulled in my company logo and a headshot (well, I needed a passport photo for a visa application and so I went to a photobooth and took a picture of _that_ photo with my phone, so it's like a manual instagram filter) from our website, and wrote up a silly bio. Lets throw that on a page a start adding classes.

{{% embed "static/profile.html.base" "html" "yes" %}}

There are two sections here, one for the header and the other for the bio. Lets open up the [Tailwind Documentation](https://tailwindcss.com/) and walk through the process of styling everything. The workflow here is to press the `/` key and type in what we are looking for to find the classes that we'll need to apply it.

### Colors

First lets tweak some overall colors. We can search for "background" to see the classes available, so lets add a few. Change the body background, maybe the text color, and the background color of the header.

```html
<body class="bg-gray-100 text-gray-800">
  <div class="bg-blue-500">
```

### Margins and Padding

Next, lets center the things in the header by added auto margins to both of the `img` tags. If we search for `margin` we'll see tons of utility classes, and we can find horizontal auto margins under `mx-auto`.

```html
<div class="bg-blue-500">
  <img src="logo.svg" class="mx-auto">
</div>
<img src="face.jpg" class="mx-auto">
```

Lets fix some sizing and shaping here. Searching for `height` or `width` we can add some classes to `logo.svg` say `h-10 w-auto` to make it `2.5rem` big.  We will also make the image `h-32` and `w-32` to shrink it down and bit.  Also search for round to find the `rounded-full` class to make the avatar rounded.

```html
<div class="bg-blue-500">
   <img src="logo.svg" class="mx-auto h-10 w-auto">
</div>
<img src="face.jpg" class="mx-auto h-32 w-32 rounded-full">
```

Finally we need to give this thing more breathing room.  Lets add some padding to the top container, and then some negative top margin on the avatar to slide it up. Since the avatar is size 32, we'll add negative 16 top margin to it. Sticking with this sizing, we'll add top 16 margin to the header div, and space it out sized 4 from the logo for 16+4 bottom margin

```html
<div class="bg-blue-500 pt-16 pb-20">
  <img src="logo.svg" class="mx-auto h-10 w-auto">
</div>
<img src="face.jpg" class="mx-auto rounded-full h-32 w-32 -mt-16">
```

## Popping

The avatar looks pretty bland now, so lets search for borders and shadows. Add `shadow-xl border-solid border-2 border-gray-300` to the avatar img to give it a little border and a box shadow, and maybe we can also add a little shadow to the colored box.

```html
<div class="bg-blue-500 pt-16 pb-20 shadow-lg">
  <img src="logo.svg" class="mx-auto h-10 w-auto">
</div>
<img src="face.jpg" class="mx-auto rounded-full h-32 w-32 -mt-16 shadow-xl border-solid border-2 border-gray-300">
```

The complete diff for the header is:

{{% diff "static/profile.html" "1" %}}

## Typography

We can center the text by adding `mx-auto` to the outer `div`, and `text-center` to both the `h1` and `h2` tags.  We'll also set a `max-w-lg` to keep the text in a smaller area.

```html
<div class="mx-auto max-w-lg">
  <h1 class="text-center">Will Schenk</h1>
  <h2 class="text-center">COO, CoFounder</h2>
```

We can adjust the text size by adding `text-4xl` and `text-xl` to the headers.

```html
  <h1 class="text-center text-4xl">Will Schenk</h1>
  <h2 class="text-center text-cl">COO, CoFounder</h2>
```

We'll change the font weight using `font-semibold` and `font-light`, and decrease the letterspacing (aka _tracking_) to `tracking-tight`

```html
  <h1 class="text-center text-4xl font-semibold tracking-tight">Will Schenk</h1>
  <h2 class="text-center text-cl font-light">COO, CoFounder</h2>
```

Finally, the verticle spacing doesn't feel great, so lets add some padding at the top and bottom.

```html
  <h1 class="pt-4 text-center text-4xl font-semibold tracking-tight">Will Schenk</h1>
  <h2 class="pb-4 text-center text-cl font-light">COO, CoFounder</h2>
```

Finally, we can open the spacing up a bit on the bio text by changing the line spacing, called _leading_ in typography lingo, and justifying the text.

```html
<p class="leading-relaxed text-justify">
```

## Directory pages

For our directory pages, we know that we are going to repeat the same element multiple times. We'll use the html `template` tag to let us edit stuff in place.  We'll define the template, a `div` to hold it in, and some short javascript to add 10 copies of it to the container.

{{% embed "static/directory.html.base" "html" %}}

Now lets get a better layout.  Lets use flexbox to lay it out. Add `flex` to the container div and we see that everyone is layed out horizontally. This is a bit intense since everything is on one row.  Lets add `flex-wrap` and `justify-around` to it also, to have it span multiple lines and equally spread out the space around each element.

```html
  <div id="container" class="flex flex-wrap justify-around">
```

Much better.  The image I'm using is huge, so lets shrink down that `img` class so we can see what we are looking at. Lets also copy the rounded and popping ideas from before.

```html
      <img src="face.jpg" class="h-32 w-32 shadow-xl border-2 border-gray-200 rounded-full">
```

Lets now fix the size of the cards themselves.  We've seen all these classes before, but I'm going to make it `w-64` and add a rounded border to it.

```html
    <div class="w-64 border-solid border border-gray-200 rounded-lg">
```

I want to center the image and have it overlap the card.  We'll do this by adding `mt-20` to the outter `div`, and `-mt-16 ml-16` to the image.  

```html
    <div class="w-64 mt-20 border-solid border border-gray-200 rounded-lg">
      <img src="face.jpg" class="h-32 w-32 -mt-16 ml-16 shadow-xl border-2 border-gray-200 rounded-full">
```

Finally we'll just center the email address.

```html
      <a class="text-center block mt-2 font-light" href="mailto:will@happyfuncorp.com">will</a>
```

## Adding more pages







## References

1. https://tailwindcss.com/
1. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template

