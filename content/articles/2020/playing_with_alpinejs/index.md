---
title: Playing with Alpine JS
subtitle: more adventures in single file coding
tags:
  - howto
  - javascript
  - onefilecoding
  - buildless
date: "2020-02-20"
draft: true
---

[AlpineJS](https://github.com/alpinejs/alpine) bills itself as "Tailwind for JavaScript".  I like Tailwind, I like simple tools, so lets give it a try!

## Template

First lets layout the structure of our html.  There's a header with a "logo" and a button, some content text, and finally what we are going to use for a modal.

{{< highlight "html" >}}
{{% raw "static/0base.html" %}}
{{< / highlight >}}

Pretty straightforward

## Lets add some alpine for that modal

Inside of `head` lets reference `tailwind` and `alpine.js` to get started.

```html
<link href="https://unpkg.com/tailwindcss@1.2.0/dist/tailwind.min.css" rel="stylesheet">
<script src="https://cdn.jsdelivr.net/gh/alpinejs/alpine@v2.0.0/dist/alpine.js" defer></script>
```

Lets add a state to the body tag:

```html
<body x-data="{open:false}">
```

and a `@click` handler to the two buttons:

```html
<button @click="open = true">
```

And then we'll use `x-show` to control the visibility of the modal.  We'll add a `@click.away` handler on the inner `div` to close the modal.

```html
<div x-show="open">
  <div @click.away="open = false">
    <p>This is going to be our modal</p>
  </div>
</div>
```

Actually, lets change the `x-show` to have a nice transition instead.  We'll have the outer div fade in, and the actually dialog zoom in a bit.

```html
<div x-show.transition.opacity="open">
  <div
	x-show.transition.scale="open"
	@click.away="open = false">
    <p>This is going to be our modal</p>
  </div>
</div>
```

## Style the modal

We want to have the who screen overlayed with a transparent background, which we'll do with an inline style since `tailwind` doesn't have something like that over the box.  We'll make it `fixed` position, pinned to the four corners using `inset-0`, and style the innter stuff using `flex`.

For the inner `div` -- the modal itself -- we'll make that `relative` positioned, a little above the background with `z-50`, and center and fix the size using `max-w-md m-auto`.  With some padding and a background we are done.

```html
<div
  class="fixed inset-0 z-40 overflow-auto flex" style="background-color: rgba(0,0,0,.1);"
  x-show.transition.opacity="open">
  <div
    @click.away="open = false"
    class="relative z-50 max-w-md m-auto flex-col bg-white p-8"
    x-show.transition.scale="open">
    <p>This is going to be our modal</p>
  </div>
</div>
```

## Final styling

Here is our final file

{{< highlight "html" >}}
{{% raw "static/2styles.html" %}}
{{< / highlight >}}

which looks like

<p>
{{% img img="final.png" style="max-width: 100%" %}}
</p>

