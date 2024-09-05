---
title: Leveraging disposability for exploration
subtitle: how to play around without leaving a mess
tags:
  - docker
  - node
  - transient
date: "2020-02-05"
---

I play around with a lot of technology to see what's out there and keep myself current, especially trying to find simpler ways to do [our work](https://happyfuncorp.com/work). One of the issues is that this can leave lots of stuff laying which can be messy.  Here are some techniques I use to keep things under control.

There are two complementary principle's that I focus on.  One is reproducibility, and the other is disposibility.  I want to be able to easily recreate what I was working on so that I don't care if I throw it away. The easier I can reproduce things the better I can revisit them -- especially if I'm deep into something when I realize that there was a fundamental misunderstanding at the beginning.

## Labnotes and scripts

The first thing is that you should get into a habit of writing everything down.  A lot of posts on this site start out as me copy and pasting commands that I'm running into a text file for later reference.  Often I find that after I get to a certain place I realize that I've over complicated something and that there is a simpler way to do something, and my workflow in that situation is to delete everything that I was working on, go back and follow the steps I was had used to get there making corrections on how I went.

Initially this is a simple text file that I copy commands into.  Depending upon how far I get, this turns into a executable shell script, a `Dockerfile`, a set of terraform provisioning files, ansible scripts or kubernetes files, etc. I prefer bash scripts overall since that's the most flexible. Since this stuff isn't in production I really don't need to upgrade in place so I can cut some corners with idempotency by, basically, throwing away the entire container/instance/vm if I've made a mess.

## `cd $(mktemp -d)`

Which means that it should be easy to delete everything. I will generally create a working directory inside of a temp folder to play around in.  This means that once I drop the idea, or that I reboot the computer the files all clean themselves up and go away. But I have a script I can follow to recreate things.

This is especially useful for cloning things out of github to really poke around in them.  Especially node projects that vomit out a massive `node_modules` directory which I don't need eating up hard disk space that I'll need to track down in some unknown future.

## Provisioning your local computer with a script

I split my computer time between a couple Pixelbooks, a MacBook Air and, well, my phone. Doing stuff on the phone using [TermUX](https://termux.com/) is possible but its a different enough environment that it's not really worth it, since most linux things don't really support arm processors in a first-class way. If you do a lot of RaspberryPI stuff maybe.

But for the laptops setting up a script that installs your base environment is super useful, and when you find a tool that you use regularly it helps to consolidate the install script in one place.  The Pixelbook in particular lends itself to this since it's so natural to delete the entire linux environment and recreate it that it makes it easy to test. For debian machines (aka the Pixelbook) this is [the script that I use](../../2019/computer_setup_script/), so on a clean VM I do:

```bash
cd /tmp ; curl https://willschenk.com/bootstrap.bash > bootstrap.bash; bash bootstrap.bash
```

And a few minutes later I'm good to go.  When there's something else that I think I'd want in my toolbox, I add it to this script and install it from there so I never need to remember how to do it again.

I'm experimenting with putting my config files in there also, or rather pulling them from a github repository. My `.emacs` file for example is a contender.  At the moment this doesn't seem to be worth the overhead but your mileage may vary.

## Docker to spin up one-off services

I was testing out how to connect [deno](https://deno.land/) to [redis](https://redis.io/) the other day, and I needed a redis server to play around with.  So I ran

```bash
docker run --rm -p 6379:6379 redis
```

And it pulled down the latest image and started it up.  One line.  When I kill that command, since I passed the `--rm` it cleans up the container afterwards and makes everything go away.

This works for other things other than redis.  As a distribution mechanism this is super cool and easy.

`docker-compose` is often more trouble than it's worth.  It's really useful when you are setting up private networks between the server or managing long term volumes, but compare how hard it was to setup `certbot` using `docker-compose` in [setting up an ipfs_node](../../2019/setting_up_an_ipfs_node/) vs [setting it up with a bash script](../server_templating_with_terraform/setup.bash). The bash script is much simpler to understand, reason about, and more importantly, tweak when you come back to it 6 month later.

## Single binary deploys

In many ways what Docker gives you is a way to treat everything as a single binary deploy.  Yes the registry makes it easier, and yes having standard ways to set network ingress and volume access control are also super amazing, but really the part where it shines is the distribution method. One of the things that's great about go and (maybe?) rust is that you get the single binary. After playing around with a lot of these different packaging systems it really makes things nicer. Docker helps with that which I think is a reason why it's so popular.

## Docker to isolate the build environment

- `node` projects require that you install `nvm` -- which needs to mess with your environment variables -- and often install stuff in the global name space (though `npx` is helping with that) and then vomit out a `node_modules`. 
- `ruby` projects imply `rbenv` -- which needs to mess with your environment variables -- and then run `bundler` and install the gems who knows where. 
- `python` requires you to install `pip` -- which also mess with your environment variables -- and frankly I don't know where it stashes its stuff, seems like `~/.local` and `/.cache` which other things use also.
- `go` is shifting things around and I think it's getting better with go modules but you still have a (frankly very weird idea) of `GOPATH` that puts everything in one honking workspace that you'll need to isolate.
- I'm sure there's something similar and exactly as annoying for `rust` and `php` projects but the same techniques should apply.

This annoys me, because it's hard to isolate everything so that I can throw it away.

One technique I'll use here is to actually work inside of a docker container to get all of this stuff out of the way. Lets say we are talking about `create-react-app`.  We can create a `Dockerfile` like this:

```Dockerfile
FROM node:11
CMD mkdir -p /app & cd /app
CMD bash
```

Then build it:

```bash
docker build . -t nodeimg
```

Then start it up as an interactive shell, connecting to the host network with the current directory mounted in `/app`:

```bash
docker run --rm -it --network host -v ${PWD}:/app nodeimg
```

And inside of it run `cd /app && npx create-react-app myapp` which will install a `cra` templated application. Inside of this you can run `yarn start` or whatever to bring up a server, use whatever editor you want on the local machine, etc.  And once you quit that shell all of the `node` stuff on your machine is gone.

By the way:

```bash
$ du -sh myapp
229M    myapp/
```

229 megabytes for an empty project!

If you are a node person anyway and already have things setup than you don't need to do this obviously.  And depending upon how your editor is setup it may make sense to have a system install.  

But imagine that you are working on a remote server for some reason and you need to get into some node stuff real quick -- how much cleaner would this be to spend your 30 minutes doing whatever and then getting out cleanly, rather than having to mess around with installing system packages!

OSX is clearly a second class citizen in the Docker world.  The filesystem performace is shockingly atrocious.  It works but... And these commands won't actually work if the volume you mount isn't in the right filesystem -- i.e. you can't develop like this in a `mktemp -d` "environment", since Docker won't let you mount files from there.

Another caveat here is that if you are on OSX this will give you a linux `node_modules`, so if there is anything that requires native libs you need to reinstall `node_modules` if you want to move locally, but you should have all the lockfiles in place to give you the same versions.

## HTTP2 requires HTTPS which requires a cert which requires a domain

We are going full encryption everywhere which is great and a lot of things refuse to work right if its not run over HTTPS. You can get around this a bit by using HTTPS=true in lot of development environments which setup a self-signed certificate which is OK-ish, but often you need a server.  I've documented how I [spin up a linode server, point a name to it, and setup nginx with certbot](../server_templating_with_terraform) earlier and this was a game changer. `terraform` provisions the server and sets up DNS for me, and then I run a script to setup nginx and install a [LetsEncrypt](https://letsencrypt.org/) certificate.

The key thing about this setup is `terraform destroy` which cleans up everything after itself.

This makes a lot of possible things easy, and as they get easier you are more likely to do it.

## Why I don't like one-click installers

A common workaround to this is to have one-click installers on DigitalOcean or Heroku or something.  I don't really like this because it hides too much stuff -- my goal often isn't to actually run [dokku](http://dokku.viewdocs.io/dokku/) or [discourse](https://www.discourse.org/) or [mastodon](https://docs.joinmastodon.org/admin/install/) or whatever, it's to explore how it works. Actually running a production system is more complicated and by assembling the pieces myself I get a better sense of what is involved. There's nothing wrong with the approach of setting up quickly and I think it's necessary for easy on-boarding to using the project.

Especially if you already have supported infrastructure it makes sense to figure out how deploy on that rather than provisioning new stuff. In terms of actual deployment we use a lot of PaaS stuff -- I for one am happy never to have to run a database server again in my life -- so understanding how to pull out the Postgres install to use CloudSQL or whatever is valuable if we do decide to adopt something.

The workflow here would be to get it working with the script, migrate those scripts to terraform to deal with the provisioning and coordination, and then use packer, ansible or kubernetes as needed to configure all the packages.  But it starts with the scripts.

## In summary

1. Write every step down in a work file and constantly iterate
2. Prefer bash scripts that assume stock OS image install
3. Treat every working directory as a temp directory
4. Isolate environmental contamination as much as possible
5. Automate provision of local environment
6. Make sure you automate tear down of cloud environment
