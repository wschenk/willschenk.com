---
title: Making charts with VueJS and no tooling
subtitle: Static files all the way
tags:
  - howto
  - vuejs
  - buildless
  - onefilecoding
  - static_sites
date: "2020-02-19"
---

I learned most of what I know about coding by looking at source code -- especially view source on a web browser. Lets see if we can bring that era back a bit by using ES modules and eschewing webpack and other bundling systems.  We will use the amazing [unpkg.com](unpkg.com) CDN to get our building blocks to assemble together.

## First get vue working

From `unpkg` we will link to tailwindcss in the `head` tag, and then `import` vuejs from using a `script type="module"` tag.

{{< highlight "html" >}}
{{% raw "static/0base.html" %}}
{{< / highlight >}}

Using `import` we are pulling the `vue.esm.browser.js` build straight from the CDN!  So easy.

## Add some charts

[chart.xkcd](https://timqian.com/chart.xkcd/) is a nifty JavaScript library that lets you make charts like the stupendious [xkcd](https://xkcd.com/). We'll use the [chart.xkcd-vue](https://github.com/shiyiya/chart.xkcd-vue) wrapper. 

[pika.dev](https://pika.dev) is an index of packages that you can import as ES modules. That's a great place to search for packages that can be `import`ed directly. So lets import `chartXkcd` and wire it up with some sample data.

{{< highlight "html" >}}
{{% raw "static/1line.html" %}}
{{< / highlight >}}

Which should generate the following image:

<p>
{{% img img="graph.png" style="max-width: 100%" %}}
</p>

## Pulling in data from a file

We can shift the code to pulling out the data from a seperate `json` file.  I'm using a trick here to wrap everything in an anonymous function so I can use `async` and `await` to load stuff from the network.  (I'm not a JavaScript developer so there could be a more awesome way to do this.

```javascript
(async () => {
  const result = await fetch( './dataset.json' )
  const data = await result.json()
  
  ... old code using the data loaded from a file ...
  
})()
```

## Consuming files that are easier to make with shell scripts

Lets write a couple bash commands to look at the number of commits in a repo organized by month.

1. Create a file with a header.
2. Get the dates of all of the commits in the log using `git log` and the `--pretty=format` command `since` a year ago.
3. Get rid of all the time after the month using sed. (The three characters before the `T` is the `-` and the two digit day.)
4. Run it through `uniq` to get the count
5. Format the output with awk, print a header using `BEGIN` and switch the order of the data and put a comma between it.

Is this the _right way_ to make a CSV file?  No.  But it's easy to copy and paste.

```bash
git log --reverse --pretty='format:%aI' --since="1 year ago"|\
sed 's/...T.*//'|uniq -c|\
awk 'BEGIN {print "Month,Commits"} // {print $2 "," $1}' > commits_by_month.csv
```

That gives us a csv file like this:

{{< highlight "csv" >}}
{{% raw "static/commits_by_month.csv" %}}
{{< / highlight >}}

And now we can hack together some JavaScript to parse this file and create a data driven `dataSet` that we'll pass off to the charting library.

{{< highlight "html" >}}
{{% raw "static/3csv.html" %}}
{{< / highlight >}}

Parsing a csv file using `split` and not bothering to do any error checking for `parseInt` is serious cowboy style coding, but it is something that we can hack together quickly. Just expect to throw it out after you get it working.  

<p>
{{% img img="monthlygraph.png" style="max-width: 100%" %}}
</p>

## Next steps

With this basic framework in place, you create simple visualizors of data in a self container file. Write some scripts that output JSON, and then move this HTML file around to parse and display them.

Everything that you need is in one file, there's no build process, you can copy and tweak it around. This is code that is meant to be deployed in on off situations, and when it break you throw it away and write it again.
