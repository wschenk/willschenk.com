title: Computer Setup Script
subtitle: Setup OSX or Linux or Chromebook quickly
date: 12-10-2019
draft: true
tags:
	- howto
	- linux
	- osx
	- automation
	- bash boilerplate
---

Putting things in scripts makes it easy to get up and running quickly.  Devops isn't just for servers or `Dockerfile`s, you can also use it for your own environment.  This is the script that I use to get my Chromebooks up and running after a wipe and how I get a new OSX machine up an running.  Lets use the [bash boilerplate](https://bash3boilerplate.sh/) to write our script.

## The strategy

1. Awesome boilerplate code to make things easier
2. Install gcc and git if needed
3. Setup git username and email
3. Setup .ssh keys (assuming you are copying from a secure store)
2. `not_installed` function, which looks to see if a command is installed and returns true if the user wants to install it.
3. `not_installed_forced` function, which always will install when we need these things.
4. `update_check` which will call `apt-get update` no more than once a session
5. `install_application` which will install the package
6. `install_`_name_ which contains specific instructions for custom packages.
7. A couple of things -- `rbenv`, `nvm`, etc mess with the environment.  For these we set the `RESET_TERMINAL` flag to true and leave the user with the nice warning.

## The packages

| name | Description |
| emacs | Amongst other things, an editor of files |
| tmux | Screen multiplexor |
| ag | Fast file searching |
| docker | Container builder and runner |
| nvm | Node version manager |
| rbenv | Ruby version manager |
| go | Golang compiler |
| hugo | Static site builder, built from source |
| heroku | Heroku toolbelt |
| atom | Atom text editor |
| gcloud | Google Cloud CLI |

## How to run

This file is mirrored at [https://willschenk.com/bootstrap.sh](https://willschenk.com/bootstrap.sh) and you can run it with the command:

```bash
$ curl -o- https://willschenk.com/boostrap.sh | bash
```

And you should be good to go!

## The code:

{{% code file="content/articles/2019/computer_setup_script/bootstrap.sh" language="bash" %}}

