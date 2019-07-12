---
title: Installing guix on IntelNUC
subtitle: shhh we're using non free
draft: true
date: 2019-07-09
tags:
  - freesoftware
  - linux
  - guix
  - docker
---

Steps:

1. Install guix in a virtual host running on the host machine
2. Pull down the latest guix and nonguix channels
3. Define an operating system configuration for the IntelNUC using non-free wifi
4. Build a boot installer disk image
5. Burn it onto a USB key
6. Disable secure booting on the NUC
7. Install guix on the NUC

We have two options of setting up the environment -- one is to install guix in a debian docker container, use that to generate the installation image and export it out.  The other is to run an actual GuixSD distrubution inside of qemu, use that the generate the installation image, and export it out.  I don't know how to make `qemu` work well on OSX so I used the docker strategy (A).  But (B) is probably nicer if you are starting with a Linux machine,

## Option A: Dockerfile for guix

We are going to put the steps for [binary installation](https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html#Binary-Installation) of Guix into a `Dockerfile`, which will be based upon `debian`.  This will all be in a container so once we are done we won't ahve anything on the host machine left over.

One thing to note is that in order to run `guix pull` we'll need to run docker with `--privileged`.  Not totally sure why, but ** mailing list links **.

{{% code file="content/articles/2019/installing_guix_on_nuc/Dockerfile" language="Dockerfile" %}}

Then we build it and start it up.

```bash
$ docker build . -t guix &&  docker run --privileged -it --rm guix
```

One of my favorite things about this command is that everything basically goes away after you exit out.  This is a play ground that will happy disappear and recreate itself as needed.

Once we are in, the first thing we need to do is to start up the `guix-daemon` inside of the container.  We don't know exactly what directory it's installed in, but there should only be one directory in `/gnu/system` that matches the glob `*guix*` at this poinnt.

```bash
# /gnu/store/*guix*/bin/guix-daemon --build-users-group=guixbuild &
```

Do a `guix pull` to make sure that everything is installed correctly.

Once this is working, set your path:

```bash
# export PATH="/root/.config/guix/current/bin${PATH:+:}$PATH"
```

You can test out the installation by `guix install hello` and then trying to run `hello`.

## Option B: Installing QEMU

You can install `qemu` and run the sample installer this way.  This is painfully slow on OSX, but works pretty well on the Chromebook.  This is because I remove `--enable-kvm` on the MacBook so I guess it falls back to software cpu emulation or something, I don't know the equivlent for how to do this on Darwin.

```bash
# apt-get install qemu
# wget https://ftp.gnu.org/gnu/guix/guix-system-vm-image-1.0.1.x86_64-linux.xz
# xz -d guix-system-vm-image-1.0.1.x86_64-linux.xz
```

And then start it up.  For OSX take out `enable-kvm`

```bash
qemu-system-x86_64 \
   -net user -net nic,model=virtio \
   -enable-kvm -m 2048 \
   -device virtio-blk,drive=myhd \
   -drive if=none,file=guix-system-vm-image-1.0.1.x86_64-linux,id=myhd
```

Once this is started up, our directions converge together below.

## Include the `nonguix` channel

As root, create a `~/.config/guix/channels.scm` file to include the `nonguix` channel:

```scheme
# cat > ~/.config/guix/channels.scm
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
       %default-channels)
```

And then run `guix pull` to update everything.  This should take a bit, but not terribly long.

```bash
# guix pull
```

## Build the install image

First we need to figure out where `nonguix` is installed.

```bash
$ ls -ld /gnu/store/*nonguix*
```

This will print out a few things, find the directory and then run the `guix system disk-image` command to create the iso.

```bash
guix system disk-image /gnu/store/n17bqqbsdz4b40vsi851cwzry0wnypfl-nonguix-ec6c0a3/nongnu/system/install.scm
```

This command will take _forever_ (around 1 hour) mainly because it's compiling the linux kernel and other fun stuff.  There isn't a substition server for the `nonguix` pacakges so everything will happen using source on your local machine.  My MacBook runneth hot.

If you get an error, make sure that the guixbuild users are added to the kvm group.  (Edit /etc/group if needed.)

When this is done, you should a path name printed that has the installation image.

```bash
$ ls -hl /gnu/store/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image
-r--r--r-- 2 root root 2.9G Jan  1  1970 /gnu/store/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image
```

## Copy the install image and a linux export to the host machine

Make sure that ssh is installed and copy to your local machine, in my case `wschenk@192.168.1.52`.  Yours should probably be somewhere else.

```bash
# guix install openssh
# export PATH=/root/.guix-profile/bin:$PATH
# scp /gnu/store/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image wschenk@192.168.1.52:/tmp
```

## Export the kernel package from the build machine

First we need to generate a signing key for our build machine, then extract the packages that we want, and move the files over to the new machine.  Lets do that generate and export now do we are ready for the import.

```bash
# guix archive --generate-key
# guix archive --recursive --export linux > linux.nar
# scp /etc/guix/signing-key.pub linux.nar wschenk@192.168.1.52:/tmp
```

## Write the ISO into the USB

And on the host machine, lets put it onto the USB stick.  If you are on OSX, [Etcher](https://www.balena.io/etcher/) is a nice app for doing this.  Otherwise you can use `dd` and if it's in `/dev/disk2`

```bash
$ sudo dd if=/tmp/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image of=/dev/disk2
```

## This doesn't work

disable secure boot

## The installation

Follow the steps as normal.  Do what you'd like here.

1. Select locale
2. Select language
3. Select graphical install
4. Select timezone
5. Format disk as required
6. Enter host name
7. Select WiFi
8. Scan for Connections, then select your network
9. Enter root password
10. Add user
11. Select say GNOME
12. Turn on SSH
13. Install the system
14. Pull out the USB key and reboot the system

## Boot and install new kernel

On the host machine, (optionally) compress linux.nar, and copy that archive and the signing public key to a USB drive.

```bash
$ cd /tmp
$ bzip2 -v linux.nar
$ mv signing-key.pub linux.nar.bz2 /Volumes/UNTITLED/
```

Then move to the target system.  First we need to be root to authorize our build key.  Then we import the kernel into our archive.

```bash
$ sudo guix archive --authorize < /media/wschenk/UNTITLED/signing-key.pub
$ bzcat /media/wschenk/UNTITLED/linux.nar.bz2 | guix archive --import
```

Then we need to edit our config.scm.

```bash
$ cp /etc/config.scm ~
$ nano ~/config.scm
```

And add `(kernel linux)` to the configuration.
## References

1. https://www.gnu.org/software/guix/download/
2. https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html#Binary-Installation
3. https://www.gnu.org/software/guix/manual/en/html_node/Installing-Guix-in-a-VM.html#Installing-Guix-in-a-VM
3. https://gitlab.com/nonguix/nonguix/blob/master/README.org
4. https://forums.intel.com/s/question/0D50P0000490X0zSAE/image-authorization-fail-system-can-not-boot-to-this-usb-device?language=en_US
5. https://lists.gnu.org/archive/html/help-guix/2016-02/msg00046.html
6. https://bbs.archlinux.org/viewtopic.php?pid=1324810#p1324810
