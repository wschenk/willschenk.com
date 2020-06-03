---
title: Playing with deno
subtitle: Rethinking package managers
tags:
  - deno
  - typescript
  - javascript
date: "2020-06-03"
---

Deno is a new V8 based TypeScript and JavaScript programming language that works like a command line web browser.  It was written by the creaters of NodeJS and seeks to fix some of the issues with Node around it's security model and pacakge management.  Lets look at building a simple webservice using redis.

## Install deno

If you are in OSX you can use `homebrew` or install from a shell installer:

```bash
curl -fsSL https://deno.land/x/install/install.sh | sh
```

I'm using [asdf](https://asdf-vm.com/#/) so it's

```bash
$ asdf plugin add deno
$ asdf install deno latest
$ asdf global deno 1.0.4
```

## Run an example

```bash
deno run https://deno.land/std/examples/welcome.ts
```

Which yields:

```bash
Download https://deno.land/std/examples/welcome.ts
Compile https://deno.land/std/examples/welcome.ts
Welcome to Deno 🦕
```

Running it again it's cached in your local directory, which defaults to `$HOME/.cache/deno`. You can look in this directory to see what's in there.

## Installing a script

We can also install this "tool" locally to make it an executable.

```
deno install welcome https://deno.land/std/examples/welcome.ts
```

This pulls down all of the dependancies and installs an executable in `~/.deno/bin`, which you may need to add to your path.

```bash
Download https://deno.land/std/examples/welcome.ts
Warning Implicitly using master branch https://deno.land/std/examples/welcome.ts
Compile https://deno.land/std/examples/welcome.ts
✅ Successfully installed welcome
/home/wschenk/.deno/bin/welcome
ℹ️  Add /home/wschenk/.deno/bin to PATH
    export PATH="/home/wschenk/.deno/bin:$PATH"
~/deno$ export PATH="/home/wschenk/.deno/bin:$PATH"
~/deno$ welcome
Welcome to Deno 🦕
```

If you delete the `~/.cache/deno` directory, the next time you run `welcome` it will download all of the code again. This is fun because you can easily distribute and install scripts as needed and it takes care of pulling down all the dependacies that are needed.

## Connecting to Redis

Lets see how to connect to redis.  We will use the [deno-redis](https://github.com/keroxp/deno-redis) package, by simply adding it as an `import` statement at the top of the file.  Then, when we run `deno run redis.ts` it will download automatically without needing to use a seperate `node_modules` directory!

We also will pull `REDIS_URL` from the environment to let us control where things are deployed later.  There's a weird quirk with the way that `deno` parses `URL` that reflects a weird way that browsers parse URLs, so we're tweaking it a little bit to make sure that we can pull out the password and hostname if they are set.

The other thing we are doing is `export`ing the `redis` object. We will use this later to build off of in another script.

[redis.ts](redis_nodeps.ts):

{{< highlight "typescript" >}}
{{% raw "redis_nodeps.ts" %}}
{{< / highlight >}}

The first time you run `deno run redis.ts` it will download all of the needed components, and then it will give you the following error:

```bash
error: Uncaught PermissionDenied: access to environment variables, run again with the --allow-env flag
```

By default deno runs everything in a sandbox environment that has no access to the local environment.  You need to specify what permissions this code -- that gets pulled down from the internet on demand -- has.  We need to give it `--allow-env` to see the environment (where you may have secured passwords stached) and, also in this case, `--allow-net` because we want our script to be able to talk to redis.

```bash
deno run --allow-env --allow-net redis.ts
Connecting to redis at 127.0.0.1:6379
error: Uncaught ConnectionRefused: Connection refused (os error 111)
```

...and we need to startup a redis server.  If you have docker installed, the easiest way to do this (for a temporary test server) is:

```bash
docker run --rm -it  -p 6379:6379 redis
```

Which means start a container based on the `redis` image, `--rm` remove it when done, `-it` give it an interactive terminal, and `-p 6379:6379` expose the local container port `6379` as `6379` on the local network.

Once done, when we run our script we get:

```bash
$ deno run --allow-env --allow-net redis.ts
Connecting to redis at 127.0.0.1:6379
Hello from Deno 1
$ deno run --allow-env --allow-net redis.ts
Connecting to redis at 127.0.0.1:6379
Hello from Deno 2
```

## Creating a webservice

This is the example that you see everywhere, except that

1. We import `redis` from our previous script
2. And we call `redis.incr` to add a counter for each "page"

[web.ts](web_nodeps.ts):

{{< highlight "typescript" >}}
{{% raw "web_nodeps.ts" %}}
{{< / highlight >}}

Lets run this now with `deno run --allow-env --allow-net web.ts`.  With you browser go to [http://localhost:8080/](http://localhost:8080/) and refresh to watch the counter go up and up.  Exiciting!

## Wrangling dependacies with `deps.ts`

As scripts get bigger we want to be able to centralize which versions of what we are using. Instead of having a `package.json` file where everything is listed, we can simply organize our project in a way that keeps it cleaner.  The standard idiom is to have a `deps.js` file that simply imports and exports things from one place.  Lets create that now and update our source files to go through that.

[deps.ts](deps.ts):

{{< highlight "typescript" >}}
{{% raw "deps.ts" %}}
{{< / highlight >}}

Then in `web.ts` change the imports to

```typescript
import { serve } from "./deps.ts";
import { redis } from "./redis.ts";
```

and in `redis.ts` change the imports to

```typescript
import { connect, RedisConnectOptions } from "./deps.ts";
```

Now everything is centralized in one place.  Running

```bash
$ deno cache deps.ts
```

Will make sure that everything in is the local cache, which we will take advantage of when building our Docker image.

## Building a docker container for deployment

When we package up our application for deployment, we don't want it to be pulling random source code from the internet when it first starts up.  We'll make that part of our build process.  Here is a simple Dockerfile that

1. Builds off of the alpine deno instance `hayd/alpine-deno:1.0.4`
2. Copies the `deps.ts` file into the image
3. Runs `deno cache deps.ts` which pulls the require dependancies into the local cache
4. Copies the rest of your source directory in
5. And defines a startup command that includes the required permissions

[Dockerfile](Dockerfile):

{{< highlight "typescript" >}}
{{% raw "Dockerfile" %}}
{{< / highlight >}}

Now from here you are ready to deploy this on whatever hosting provider you need.  It needs a redis instance that it will find by looking at `REDIS_URL` in the environment, which is the standard that `heroku` or `dokku` (and probably others) use.

Lets test it out locally first.  You'll need to find your ip address which on linux you can do with `hostname -I`. Change the `REDIS_URL` to point to your local instance.

```bash
$ docker build . -t deno
$ docker run --rm -it --env REDIS_URL=redis://172.17.0.1:6379 -p 8080:8080 deno

```

Now when you go to [http://localhost:8080/](http://localhost:8080/) you should see deno running in your local container.

## Thoughts

From here it will be straightforward to do a `git push heroku master` or whatever to deploy on your provider of choice.  

Deno is like a web browser for the command line.  One thing that webbrowsers do amazingly well is the distrubtion of applications -- just go to a URL and you have it on your machine.  `deno install` brings some of that magic into the command line.  The ecosystem is still very young, but with ES modules being the extension mechanism that can pull code from wherever it lives on the internet you can imagine a much more distributed application mechanism that doesn't rely on any particular centralized service.
