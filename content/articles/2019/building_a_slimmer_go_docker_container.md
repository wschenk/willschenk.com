---
title: Building a slimmer go Docker container
subtitle: All we need is the binary
tags:
  - howto
  - docker
  - golang
date: "2019-04-09"
---

Go binaries are self contained, which means that they don't need anything special installed in the environment to deploy them.  When people make `Dockerfiles` to build go projects, they often include the the golang compilers and build tools, which isn't necessary for running the container.  I'm going to use [healer](https://github.com/somarat/healer) Docker container that "Automatically heal docker containers that report themselves unhealthy" as an example of reducing the image size from 648MB to 17MB.

<!--more-->

```bash
$ docker images  |grep healer
somarat/healer             latest              76313596c92a        15 months ago       642MB
wschenk/healer             latest              2502c0b68d5e        About a minute ago  17.4MB
```

## Original Dockerfile

The original file uses a golang image, copies everything into the work dir, installs some additional packages
a configuration and then uses the `go-wrapper` command to build and run the app.

```Dockerfile
FROM golang:1.9.2-alpine3.7

WORKDIR /go/src/app
COPY . .

RUN apk --no-cache add -t build-deps build-base git \
	&& apk --no-cache add ca-certificates \
  && git config --global http.https://gopkg.in.followRedirects true

RUN go-wrapper download
RUN go-wrapper install

CMD ["go-wrapper", "run"]
```

Lets change it around (also because new version of the golang images don't have `go-wrapper` anymore!)

## Build and deploy containers

The basic idea is that we first bring down a full development environment for the code, and build it using the standard ways of building go applications.  Once that is done we will use a very slimmed down container and copy the built artifacts to it.

While we still should create a `.dockerignore` file to slim down the docker build context, none of the files will inadvertantly make it to the final image.  (In this case we only really need to copy in one file at all, `healer.go`, but we're copying in everything.)

We skip the additional package installs completely because they aren't needed.  We are relying upone the golang image being up to date and copying the certificates there over.

To do the build, we simply run `go get -d` to install the depenancies and then `go build` to create final binary over.

```Dockerfile
FROM golang:1.12.3-stretch
MAINTAINER Will Schenk <wschenk@gmail.com>

# Get the TLS CA certificates, they're not provided by busybox.
RUN apt-get update && apt-get install -y ca-certificates

# Copy the single source file to the app directory
WORKDIR /go/src/app
COPY . .

# Install depenancies
RUN go get -d

# Build the app
RUN go build

# Switch to a small base image
FROM busybox:1-glibc
MAINTAINER Will Schenk <wschenk@gmail.com>

# Copy the binary over from the deploy container
COPY --from=0 /go/src/app/app /usr/bin/healer

# Get the TLS CA certificates from the build container, they're not provided by busybox.

COPY --from=0 /etc/ssl/certs /etc/ssl/certs

CMD healer
```

Then all we need to do is build and tag it:

```bash
docker build . -t wschenk/healer:latest
```

## Verifying

I'm using my docker hub user id, you should replace it with your own.

To test it out, you can start it up and make sure that you give it access to the `docker.sock` socket file.

```bash
docker run -d --name healer -v /var/run/docker.sock:/tmp/docker.sock --restart unless-stopped wschenk/healer
```

And then check out the logs:

```bash
$ docker logs healer
2019/04/09 20:07:05 Monitoring container health
```

To make sure that everything is good.

Finally, push it to the hub to be able to access it from other machines!

```bash
docker push wschenk/healer
```
