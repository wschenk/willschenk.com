---
title: Playing with tailwind
subtitle: An excersize in minimilism
tags:
  - howto
  - tailwind
date: "2020-02-01"
---

We are going to use [tailwindcss](https://tailwindcss.com/) to build a site. We will start with a blank folder and a static HTML file to work on the designs, and then slowly bring in other tools when needed to add in functionality.

The site that we are going to build is a company directory that pulls in data from Google Apps Suite. The goal here is to build the simplest thing possible and not get lost in the tooling. We will start with a static site and explore different ways to pull in the data and style it.

1. First go through styling a page with tailwind
2. Then we'll introduce the `template` tag to style repeated elements
3. Then we'll build a _Single Page App_ within a single HTML file with no tooling.

## Create the project folder and base HTML file

```bash
mkdir projectname && cd projectname
```

Now we will create a base html file that we'll use to start styling. We are going to include the full `tailwind.min.css` file from a CDN so there's no build process here.

{{% embed "static/base.html" "html" %}}

Let's start up a preview server by running `npx live-server` which should open up a browser window which will update files.  You should see a window open up.  If you edit some of the text, you should see it updated in the browser once you save. So far so good. All we've needed is an html file and node installed on the machine.

## The profile page

I pulled in my company logo and a headshot (well, I needed a passport photo for a visa application and so I went to a photobooth and took a picture of _that_ photo with my phone, so it's like a manual Instagram filter) from our website, and wrote up a silly bio. Let's throw that on a page a start adding classes.

Lets build this:

{{% img img="profile_done.png" style="max-width: 600px" %}}

{{% embed "static/profile.html.base" "html" "yes" %}}

There are two sections here, one for the header and the other for the bio. Let's open up the [Tailwind Documentation](https://tailwindcss.com/) and walk through the process of styling everything. The workflow here is to press the `/` key and type in what we are looking for to find the classes that we'll need to apply it.

## Colors

First, let's tweak some overall colors. We can search for "background" to see the classes available, so let's add a few. Change the body background, maybe the text color, and the background color of the header.

```html
<body class="bg-gray-100 text-gray-800">
  <div class="bg-blue-500">
```

### Margins and Padding

Next, let's center the things in the header by added auto margins to both of the `img` tags. If we search for `margin` we'll see tons of utility classes, and we can find horizontal auto margins under `mx-auto`.

```html
<div class="bg-blue-500">
  <img src="logo.svg" class="mx-auto">
</div>
<img src="face.jpg" class="mx-auto">
```

Let's fix some sizing and shaping here. Searching for `height` or `width` we can add some classes to `logo.svg` say `h-10 w-auto` to make it `2.5rem` big.  We will also make the image `h-32` and `w-32` to shrink it down and bit.  Also search for round to find the `rounded-full` class to make the avatar rounded.

```html
<div class="bg-blue-500">
   <img src="logo.svg" class="mx-auto h-10 w-auto">
</div>
<img src="face.jpg" class="mx-auto h-32 w-32 rounded-full">
```

Finally we need to give this thing more breathing room.  Let's add some padding to the top container, and then some negative top margin on the avatar to slide it up. Since the avatar is size 32, we'll add negative 16 top margin to it. Sticking with this sizing, we'll add top 16 margin to the header div, and space it out sized 4 from the logo for 16+4 bottom margin

```html
<div class="bg-blue-500 pt-16 pb-20">
  <img src="logo.svg" class="mx-auto h-10 w-auto">
</div>
<img src="face.jpg" class="mx-auto rounded-full h-32 w-32 -mt-16">
```

## Popping

The avatar looks pretty bland now, so let's search for borders and shadows. Add `shadow-xl border-solid border-2 border-gray-300` to the avatar img to give it a little border and a box shadow, and maybe we can also add a little shadow to the colored box.

```html
<div class="bg-blue-500 pt-16 pb-20 shadow-lg">
  <img src="logo.svg" class="mx-auto h-10 w-auto">
</div>
<img src="face.jpg" class="mx-auto rounded-full h-32 w-32 -mt-16 shadow-xl border-solid border-2 border-gray-300">
```
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

Finally, the vertical spacing doesn't feel great, so let's add some padding at the top and bottom.

```html
  <h1 class="pt-4 text-center text-4xl font-semibold tracking-tight">Will Schenk</h1>
  <h2 class="pb-4 text-center text-cl font-light">COO, CoFounder</h2>
```

Finally, we can open the spacing up a bit on the bio text by changing the line spacing, called _leading_ in typography lingo, and justifying the text.

```html
<p class="leading-relaxed text-justify">
```

For everything is

{{% diff "static/profile.html" "final" %}}

## Directory pages

For our directory pages, we know that we are going to repeat the same element multiple times. We'll use the html `template` tag to let us edit stuff in place.  We'll define the template, a `div` to hold it in, and some short javascript to add 10 copies of it to the container.

{{< img img="directory_done.png" style="max-width: 600px" >}}

{{% embed "static/directory.html.base" "html" %}}

Now let's get a better layout, using flexbox to lay it out. Add `flex` to the container div and we see that everything is laid out horizontally. This is a bit intense since everything is on one row.  Let's add `flex-wrap` and `justify-around` to it also, to have it span multiple lines and equally spread out the space around each element.

```html
  <div id="container" class="flex flex-wrap justify-around">
```

Much better.  The image I'm using is huge, so let's shrink down that `img` class so we can see what we are looking at. Let's also copy the rounded and popping ideas from before.

```html
      <img src="face.jpg" class="h-32 w-32 shadow-xl border-2 border-gray-200 rounded-full">
```

Let's now fix the size of the cards themselves.  We've seen all these classes before, but I'm going to make it `w-64` and add a rounded border to it.

```html
    <div class="w-64 border-solid border border-gray-200 rounded-lg">
```

I want to center the image and have it overlap the card.  We'll do this by adding `mt-20` to the outer `div`, and `-mt-16 ml-16` to the image.  

```html
    <div class="w-64 mt-20 border-solid border border-gray-200 rounded-lg">
      <img src="face.jpg" class="h-32 w-32 -mt-16 ml-16 shadow-xl border-2 border-gray-200 rounded-full">
```

Finally we'll just center the email address.

```html
      <a class="text-center block mt-2 font-light" href="mailto:will@happyfuncorp.com">will</a>
```
## Variable templating

Before we go down the route of setting up webcomponents and pulling in a bunch of other files, let's change the code so that instead of duplicating the same template element each time, we drive it from an array.

{{% diff "static/directory.html" "final" %}}

This looks like it will get out of hand a bit as we add more, say `a` tags, but it's a simple way to get going.

## Adding more pages

Now we can add different pages to the site.  We'll start with a boiler plate that contains all of our previous code in a template. On the top we have the header, and a simple _sidebar_ that we'll wire up to view things on the page.

```html
<body class="bg-gray-100 text-gray-900">
  <div class="bg-blue-500 pt-16 pb-20 shadow-lg">
    <img src="logo.svg" class="mx-auto h-10 w-auto">
  </div>
    
  <div class="flex">
    <div class="w-64 p-4">
      <a href="#" class="block">Login</a>
      <a href="#" class="block pt-4">Directory</a>
      <a href="#" class="block pt-4">Profile</a>
    </div>
    <div id="root" class="w-full"></div>
  </div>
  
  <template id="login">
    <button class="m-4 px-4 py-2 bg-blue-500 text-gray-100 rounded">Login</button>
  </template>
```

If you want to see the starting code, it's [`static/index.html.base`](static/index.html.base).  Let's get into the JavaScript!

First we are going to add some click handlers to our sidebar, using good old `onClick`.

Then we have three functions that clear out the `root` dom element, pull the content from the `template` tags, adjust as needed and add to the root container.

{{% diff "static/index.html" "final" %}}

## Conclusion

From here the next step in organization would be to split these templates out into their own webcomponents. We'll be building more once we get data from an external source, and that will make the organization complicated. But for now, this works for demoable purposes and all we've had to use to make this all happen in a code editor. No tooling, compiling, and all of the code is self-contained and ready for someone to start hacking away at it.

The design and design implementation process with tailwind should also be called out for being such a pleasure. Editing one file, our choices contrained by what's available so we don't get too crazy -- I especially like that padding and margins aren't pixel based, so you get to be forced between a couple options and one is generally better than the other. You feel guided but someone else's experience.

I also really like that the HTML and the styling are together so you aren't flipping between two different mindsets. That makes it possible to resist the over generalization temptation of CSS to try and build reusable components, which you always end up needing to tweak in any case.  Sure you have to do things multiple times, but since it's so easy to do it you come out way ahead with the added simplicity and lower cognitive surface area.  Would recommend.

## References

1. https://tailwindcss.com/
1. https://developer.mozilla.org/en-US/docs/Web/HTML/Element/template

