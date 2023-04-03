---
title: Book Image Shortcode for Hugo
subtitle: Lets link to bookshop
tags:
  - hugo
date: "2020-04-28"
aliases:
  - "/articles/2020/book_images_shortcode"
---

[HappyFunCorp](https://happyfuncorp.com/) helped build [Bookshop.org](https://bookshop.org/) which is an online bookstore that distributes 30% of the book profits to independent bookstores in your area. Basically it's a place that people can link to that isn't Amazon, and that will kick some money to your local community.

I wanted to find a way to easily create links to these product pages, and show images if needed. There's no API to access bookshop (I asked on our internal dev channel) but I know how to write a script so I cobbled something together and made a `hugo` shortcode to render stuff on my blog.

<code>&#123;&#123;< bookshop isbn="9780394719856" />&#125;&#125;</code>

Which will generate this, a embedded link to the product page of the book with the title pulled from the server:

{{< bookshop isbn="9780394719856" />}} is a great book.

Or

<pre><code>&#123;&#123;< bookshop isbn="9780394719856" img="left">&#125;&#125;
Here are my thoughts about this book
&#123;&#123;< /bookshop >&#125;&#125;</code>
</code></pre>

{{< bookshop isbn="9780394719856" img="left">}}
Here are my thoughts about this book.
{{< /bookshop >}}

We are going to use a two step process here. The first is to use a bash script to pull the data from the site and store it in `json` and `jpg`, and then a shortcode to format this in the webpage. Most of my posts are directories, so this will be part of the page bundles.

## Get the book information

Since there's no API and I want to keep this simple, our strategy is to:

1. Hit the search page with the ISBN
2. Use `awk` to pull put the link from the search results.
3. Load the product page.
4. Use an unholy combination of grep, sed, and awk to pull out the meta data.
5. Store this in `${ISBN}.json`
6. Download the cover image in `${ISBN}.jpg`

[bookshop_lookup.sh](bookshop_lookup.sh):

{{< highlight "bash" >}}
{{% raw "bookshop_lookup.sh" %}}
{{< /highlight >}}

## The shortcode

I'm not an expert in hugo shortcodes, but this is what we're doing.

1. Look for the file `${ISBN}.json` in the page bundle.
2. Show a message if we don't find it.
3. Pull in data into a map using `$jsonFile.Content | unmarshal`
4. If we are showing the cover image, choose which set of bootstrap utility classes we want to use.
5. Wrap the output in `<p class="clearfix">` to deal with the overfloat. Can't believe I'm still doing this.
6. Link the image or the title to the bookshop product page, data pulled from the json file.
7. Add the `{{ .Inner }}` content inside of the tag.

`bookshop.html`:

/link to bookship.html/ TODO

Example output:

{{< bookshop isbn="9780231105958" img="right" >}} Dui, id ornare arcu odio ut sem nulla pharetra diam sit amet nisl suscipit adipiscing bibendum est ultricies integer quis auctor! Odio eu feugiat pretium, nibh ipsum consequat nisl, vel. Ac tortor dignissim convallis? Tincidunt nunc pulvinar sapien et ligula ullamcorper? Egestas diam in arcu cursus euismod quis viverra nibh cras pulvinar mattis nunc, sed blandit libero volutpat sed cras ornare arcu dui vivamus?
{{< /bookshop >}}

## References

1. https://shindakun.dev/posts/adding-a-book-cover-shortcode-for-hugo/
