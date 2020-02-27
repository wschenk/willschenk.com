---
title: Deno the next NodeJS
subtitle: Rethinking package managers
tags:
  - deno
  - javascript
date: "2020-02-27"
draft: true
---


Install deno

curl -fsSL https://deno.land/x/install/install.sh | sh

Run an example

```
deno https://deno.land/std/examples/welcome.ts
```

Which yields:

```
Download https://deno.land/std/examples/welcome.ts
Compile https://deno.land/std/examples/welcome.ts
Welcome to Deno 🦕
```

Running it again it's cached in your local directory, which defaults to `$HOME/.cache`. You can look in this directory to see what's in there.

We can also install this "tool" locally to make it an executable.  

```
deno install welcome https://deno.land/std/examples/welcome.ts
```

This pulls down all of the dependancies and installs an executable in `~/.deno/bin`, which you may need to add to your path.

If you delete the '~/.cache/deno` directory, the next time you run `welcome` it will download all of the code again.
