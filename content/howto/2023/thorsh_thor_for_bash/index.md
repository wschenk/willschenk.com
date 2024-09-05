---
title: "Thorsh: Thor, but for bash"
subtitle: simple bash cli scripting
tags:
    - bash
    - script
    - thorsh
date: 2023-07-12
---

Like [Thor](http://whatisthor.com), but for bash!

Check it out [on github](https://github.com/wschenk/thorsh)

## Example

```bash
#!/bin/bash

DIR="${BASH_SOURCE%/*}"
if [[ ! -d "$DIR" ]]; then DIR="$PWD"; fi
. "$DIR/thorsh"

add_function hello "Says hello"
function hello() {
    echo Hello, World
}
    
add_value type=x11 "Server type"
add_value image=debian-12 "Image name"

add_function create name "Create a new server"
function create() {
    echo Called create
    echo "name   = " ${name}
    echo "type   = " ${type}
    echo "image  = " ${image}
}

thorsh "$@"
```

Which gives you

```bash
$ ./example hello
Hello, World

$ ./example create server_name --image ubuntu
Called create
name   =  server_name
type   =  x11
image  =  ubuntu

$ ./example help
Usage: example [OPTIONS] COMMAND [args]

Commands
     help                  Show the help command
     hello                 Says hello
     create name           Create a new server

Options
     --type=x11            Server type
     --image=debian-12     Image name
```

## Quick Start

```bash
curl -L https://github.com/wschenk/thorsh/raw/main/thorsh > thorsh 
chmod +x thorsh
thorsh template my_script
vi my_script
```

*`thorsh` needs to be in the same directory as your script*

## Usage

```bash
bash thorsh help
Usage: thorsh [OPTIONS] COMMAND [args]

Commands
     help                  Show the help command
     template name         Create a new script
     inline name           Make a throsh script self-contained
```

### template

To generate a new template file

### inline

Merges the `thorsh` script with the user script, which then is standalone

```bash
bash thorsh inline example > example.inlined
```

## Why?

I've been doing a bunch of server scripting lately, and I want to
reach for Thor to help sort out some command options. But its a bit of
a pain to get a ruby environment setup, especially in a Docker
container where it's not the primary purpose.

But I missed the simple ways of defining a method with help and then
implementing it.

## How does it work?

Calling `add_flag`, `add_option` and `add_function` puts stuff into a
bunch of bash 3 compatible arrays.  Then the `thorsh` at the end of
your script does this:

```bash
function thorsh() {
    eval $(generate_options_parser)
}
```

Which iterates over those arrays creating bash code to generate a
while/case loop over all of the input arguments.  It sets what needs
to be set in the environment and then executes the function in `cmd`.

# Usage

```bash
thorsh template new_script
```

## add_flag key Usage

For adding single, true/false things.  (Either the string "true" or
"")

```bash
add_flag verbose "Verbose flag"
```

`verbose` default to "".

```bash
add_flag logging=true "Verbose flag"
```

## add_value key Usage

Adding a 2 parameter value, something like `--log_level debug`

```bash
add_value log_level=warn "Logging level (default: warn)
```

So:

```bash
example
```

`log_level` will be `warn`

```bash
example --log_level debug
```

`log_level` will be `debug`

## add_function name arg1 arg2 argN "Usage"

Define a function with a varible number of (fixed) arguments.

The basic is

```bash
add_function hello "Prints hello world"
function hello() {
    echo Yo
}
```

And the example above

```bash
add_function name arg1 arg2 argN "Usage"
function name() {
   echo arg1 = #{arg1}
   echo arg2 = #{arg2}
   echo argN = #{argN}
}
```

# Inspired by

- [Ruby's Thor](http://whatisthor.com)
- [How Best to Include Other Scripts](https://stackoverflow.com/a/12694189)
- [Metaprogramming with Bash](https://binkley.blogspot.com/2016/05/metaprogramming-with-bash.html)
- [bash-boilerplate: bash-subcommands](https://github.com/xwmx/bash-boilerplate/blob/master/bash-subcommands)
