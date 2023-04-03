---
title: Styling Hugo Diffs
subtitle: Showing just what you changed
tags:
  - hugo
date: "2020-01-31"
aliases:
  - "/articles/2020/styling_hugo_diffs"
---

I often want to show small changes I'm making to a file and it would be nice for hugo to support styling patches directly. Lets see what we can do to make this process easier.

Lets take the example of _create node package.json file and add the following scripts_ worflow.  How can we say this different than "copy this into your `package.json` file"?

## Create sample steps

Lets first create the file using `npm init -y` and then immediately `cp package.json package.json.base`. In this case, we want to add some `script` attributes, like so:

```js
  "scripts": {
    "build:eleventy": "eleventy",
    "serve:eleventy": "eleventy --serve",
    "debug:eleventy": "DEBUG=* eleventy"
  },
```

So we can now look at the differences of the files using the `diff` command. I'm going to have it user the unified format showing 1 line of code on either side.

```bash
diff -U 1 package.json.base package.json
```

Which outputs:

```patch
--- package.json.base	2020-01-30 14:38:18.933332377 -0600
+++ package.json	2020-01-30 15:43:37.174332377 -0600
@@ -6,3 +6,5 @@
   "scripts": {
-    "test": "echo \"Error: no test specified\" && exit 1"
+    "build:eleventy": "eleventy",
+    "serve:eleventy": "eleventy --serve",
+    "debug:eleventy": "DEBUG=* eleventy"
   },
```

This seems a little better, but how do we keep it up to date?

## Bash script

We are going to write a bash script to parse though a files. First lets do that and make sure that we can pull out the base name of the file.

{{% diff "update_patches.sh" %}}

That seems to work, now lets loop over the files that match the base name, skipping the files that we don't want. Once we get to the `.base` file, we know that we are at the end of our patching, and we need to compare the last stage with the original file instead, so we'll check for that and update the variables so that the `stage` file is actually the current one.

{{% diff "update_patches.sh" "1" %}}

The logic seems to work, so lets add the actual diff creation. But the diff files match our pattern, so we'll need to add a check to skip over them when we loop over the glob!

{{% diff "update_patches.sh" "final" %}}

## Usage

The idea here is that you are working on an explainable chunk of work.  You start by getting a base file that everyone can work off of.  For example, run `npm init -y` and then copy that file to `package.json.base`.  Then make your changes and run `update_patches.sh` to generate the patch, which will initially just compare `package.json.base` and `package.json`

Later you want to make another change to the file.  Before you edit `package.json` make a copy to `package.json.1`, and make your changes again.  Then run `update_patches.sh` which will regenerate all the diffs for each file.  Then write about it, and repeat the process as necessary.

## Displaying the patch files easily

What I want to do in while writing markdown is to do something like

_If we want to show the base file_:

<code>&lbrace;&lbrace; diff "package.json" &rbrace;&rbrace;</code>

_text here_

<code>&lbrace;&lbrace; diff "package.json" "1" &rbrace;&rbrace;</code>

_text here_

<code>&lbrace;&lbrace; diff "package.json" "final" &rbrace;&rbrace;</code>

in my content file, and which will figure out the correct diff to inline it.

Here is a [hugo short code](/diff.html) that you can drop in your `/layouts/shortcodes/` folder that will make that possible.

Embedding shortcodes as an example inside of hugo is a pain, so I'll walk you through the logic but we wont' be looking at the code.

1. Pull out the parameters to make it easier to work with.
2. `$diff` is going to contain the file contents, initialize it to blank in a scope that everyone can see.
3. If `$stage` is set and is `final`, we append ".diff" to a file -- which we assume is in the same directory as the Page that is calling the short code -- and `readFile`.
4. If `$stage` is set but not final, append ".$stage.diff" to the filename and load.
5. If `$stage` is blank load "$filename.diff". This is to avoid expanding to `..diff` which has an extra `.`.
6. Markdownify a link to the file directly and pass to markdownify
7. Markdownify the actual file, formatting is as a `diff`

## What it looks like in action

Lets first start out by creating an empty project:

1. `mkdir projectname && cd projectname`
2. `npm init -y`
3. Add the following script entries to the generated `package.json`

{{% diff "package.json" "1" %}}

Oh, we forgot to install `eleventy`!  Let's do that now with `npm install --save-dev @11ty/eleventy` which should modify your `package.json` like so:

{{% diff "package.json" "final" %}}
