---
title: Installing guix on IntelNUC
subtitle: using the hardware you have, even if we are nonfree
date: 2019-07-25
tags:
  - freesoftware
  - guix
  - docker
aliases:
  - "/articles/2019/installing_guix_on_nuc/"
---

I've been getting into [Guix](https://guix.gnu.org/) and [Emacs](https://www.gnu.org/software/emacs/) lately, going back to my Free Software roots. It's amazing. Guix is a functional package manager that you can use on top of a linux distribution to have repeatable and rollbackable builds. Guix System is a distrubution that's Guix all the way down.

I have an Intel NUC lying around that I wanted to use, so this is my effort to get a working Guix System installation on it. This post took me almost 14 days to write, because I wanted to use the WiFi interface rather than plugging it into my router directly, and so I had to build a custom kernel with non-free code. If you have an ethernet cord, or your hardware is supported by the [linux-libre](https://en.wikipedia.org/wiki/Linux-libre) kernel this is all overkill and just follow the [Guix System installer instructions](https://guix.gnu.org/manual/en/html_node/USB-Stick-and-DVD-Installation.html#USB-Stick-and-DVD-Installation).

I should reiterate that if a) I plugged into the router or b) had the right network hardware that linux-libre supported this guide would be much much shorter, and this was largely an excersize to force me to get into the details of how Guix System worked. This doesn't, you know, _make sense_ as a thing to do. Just plug in ethernet.

## Overview

This is our strategy:

1. Install guix in a virtual host running on the host machine
2. Pull down the latest guix and nonguix channels
3. Define an operating system configuration for the IntelNUC using non-free wifi
4. Burn it onto a USB key
5. Disable secure booting on the NUC
6. Boot off of the USB
7. Running `guix system init` to put the new operating system

We have three options of setting up the environment -- one is to install guix ontop of a current Linux installation, another one is to install guix in a debian docker container, use that to generate the installation image and export it out. The other is to run an actual Guix System distrubution inside of qemu, use that the generate the installation image, and export it out. I don't know how to make `qemu` work well on OSX so I used the docker strategy (B). But (A) or (C) is probably nicer if you are starting with a Linux machine.

## Option A: Install guix ontop of Linux

Follow the [binary installation instructions](https://guix.gnu.org/manual/en/html_node/Binary-Installation.html#Binary-Installation) from the Guix documentation to get Guix installed in your local environment. This will leave guix available on your machine, which could be great depending upon your use case. I'm doing this on a OSX machine which won't work.

## Option B: Dockerfile for guix

We are going to put the steps for [binary installation](https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html#Binary-Installation) of Guix into a `Dockerfile`, which will be based upon `debian`. This will all be in a container so once we are done we won't ahve anything on the host machine left over.

One thing to note is that in order to run `guix pull` we'll need to run docker with `--privileged`. Not totally sure why, but [here is a list to the mailing list discussion about it.](https://lists.gnu.org/archive/html/guix-devel/2017-11/msg00258.html) This `Dockerfile` is basically a simple rewrite of the installer script, that helped me understand how guix was setup under the hood.

{{< highlight "Dockerfile" >}}
{{% raw "Dockerfile" %}}
{{< /highlight >}}

Then we build it and start it up.

```bash
$ docker build . -t guix &&  docker run --privileged -it --rm guix
```

One of my favorite things about this command is that everything basically goes away after you exit out. This is a play ground that will happy disappear and recreate itself as needed. You might not want to have `--rm` flag since once you shut down or exist the container it will delete everything, but I like to have things clean up after themselves.

Once we are in, the first thing we need to do is to start up the `guix-daemon` inside of the container.

```bash
# ~/.config/guix/current/bin/guix-daemon --build-users-group=guixbuild &
```

Do a `guix pull` to make sure that everything is installed correctly.

Once this is working, set your path:

```bash
# export PATH="/root/.config/guix/current/bin${PATH:+:}$PATH"
```

You can test out the installation by `guix install hello` and then trying to run `hello`.

## Option C: Installing QEMU

You can install `qemu` and run the sample installer this way. This is painfully slow on OSX, but works pretty well on the Chromebook. This is because I remove `--enable-kvm` on the MacBook so I guess it falls back to software cpu emulation or something, I don't know the equivlent for how to do this on Darwin.

```bash
# apt-get install qemu
# wget https://ftp.gnu.org/gnu/guix/guix-system-vm-image-1.0.1.x86_64-linux.xz
# xz -d guix-system-vm-image-1.0.1.x86_64-linux.xz
```

And then start it up. For OSX take out `enable-kvm`

```bash
qemu-system-x86_64 \
   -net user -net nic,model=virtio \
   -enable-kvm -m 2048 \
   -device virtio-blk,drive=myhd \
   -drive if=none,file=guix-system-vm-image-1.0.1.x86_64-linux,id=myhd
```

Once this is started up, our directions converge together below. However, this is painfully slow on OSX so I don't think it's a viable option.

## Include the `nonguix` channel

As root, create a `~/.config/guix/channels.scm` file to include the `nonguix` channel:

```scheme
# cat > ~/.config/guix/channels.scm
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix"))
       %default-channels)
```

And then run `guix pull` to update everything. This should take a bit, but not terribly long.

```bash
# guix pull
```

## Build the boot image

Now we define the operating system that we will eventually install. Copy this [config.scm](config.scm) file into the docker container:

{{< highlight "scheme" >}}
{{% raw "config.scm" %}}
{{< /highlight >}}

And build the image with

```bash
$ guix system disk-image config.scm
```

This command will take _forever_ (around 1 hour) mainly because it's compiling the linux kernel and other fun stuff. There isn't a substition server for the `nonguix` pacakges so everything will happen using source on your local machine. My MacBook runneth hot.

If you get an error, make sure that the guixbuild users are added to the kvm group. (Edit /etc/group if needed.)

When this is done, you should a path name printed that has the installation image.

```bash
$ ls -hl /gnu/store/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image
-r--r--r-- 2 root root 2.9G Jan  1  1970 /gnu/store/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image
```

## Copy the install image and a linux export to the host machine

Make sure that ssh is installed and copy to your local machine, in my case `wschenk@192.168.1.52`. Yours should probably be somewhere else.

```bash
# guix install openssh
# export PATH=/root/.guix-profile/bin:$PATH
# scp /gnu/store/5v93jajj81mjfpp0lvkx61yk89r572cf-disk-image wschenk@192.168.1.52:/tmp/myguix.iso
```

## Write the ISO into the USB

And on the host machine, lets put it onto the USB stick. If you are on OSX, [Etcher](https://www.balena.io/etcher/) is a nice app for doing this. Otherwise you can use `dd` and if it's in `/dev/disk2`

```bash
$ sudo dd if=/tmp/myguix.iso of=/dev/disk2
```

## Booting the InterNUC off of the USB key

As documented [here](https://forums.intel.com/s/question/0D50P0000490X0zSAE/image-authorization-fail-system-can-not-boot-to-this-usb-device?language=en_US):

> 1. At BIOS POST (i.e. when the "Intel NUC" splash screen appears), rapidly press the F2 key over and over until the BIOS Setup (Visual BIOS) display appears.
> 2. Click on Advanced, then Boot and then select the Secure Boot tab.
> 3. Uncheck the Secure Boot option.
> 4. Press F10 and then select Yes to save this change and reboot the system.

## The installation

Now you should boot up off of the USB key and have guix running on your system! The `root` user doesn't have a password and neither does the user account defined.

`C-Alt-F2` will switch to a console. Log in as `root` here (without password), and set the password for your user, in my case `passwd wschenk`.

`C-Alt-F7` will go back to the Gnome login screen, where you'll be able to login as your user if you want to use Gnome.

## Setting up WiFi

`sudo rfkill unblock all` will turn on your network card, which you can figure by going to the `Activities` menu and selecting `Settings`. You need to enable the interface in order for the Gnome network manager to be able to connect to WiFi. Go to a terminal and `ping 1.1.1.1` to see if you can connect to the internet!

To do this in terminal (you can switch with `C-Alt-F2` the steps are:

1. `rfkill unblock all`
2. `nmcli device wifi` to list out the available SSIDs
3. `nmcli device wifi connect HappyFunCorp password mysekretpassword` to actually make the connection.
4. `ping 1.1.1.1` to verify that things are working

## Preparing the target disk

The easiest way is probably to use `cfdisk` or `GNOME Disk` to format your target drive. I'm going to walk through using the CLI to do this, but it doesn't really matter what you use.

First run `lsblk` to see which devices are on your system.

```bash
wschenk@intelnuc ~$ lsblk
NAME        MAJ:MIN RM   SIZE RO TYPE MOUNTPOINT
sda           8:0    1  14.9G  0 disk
├─sda1        8:1    1   5.4G  0 part
└─sda2        8:2    1    40M  0 part
nvme0n1     259:0    0 465.8G  0 disk
```

From there we can use `fdisk` to create the partitions on, in my case, `/dev/nvme0n1`.

`n` to create the first partition for with a size of `+100M`. I don't know if this is big or small, but seemed fine.

`t` to change the partition type, select `1` for `EFI System`.

`n` to create another partition for the rest of the disk.

`t` to change the partition type, select `20` for `Linux Filesystem`

`w` to write the partition table.

Run `sync` just to be safe.

Lets setup the first partition to be formatted as FAT32 and be bootable, and mount it on `/boot/efi`

```bash
# guix install dosfstools parted
# export PATH=/root/.guix-profile/sbin:$PATH
# mkfs.vfat -F32 /dev/nvme0n1p1
# parted /dev/nvme0n1 set 1 esp on
# mount /dev/nvme01n1p1 /boot/efi

```

Initialize the file system, which in my case is the 2nd partition, and label it as `guix`, and then mount it on `/mnt`.

```bash
# mkfs.ext4 -L guix /dev/nvme0n1p2
# mount LABEL=guix /mnt
```

## Edit `config.scm` file

You should find the `config.scm` file that you built the system with in `/etc/config.scm`. If not bring it over from the internet, and edit it to uncomment out the correct bootloader, and specify the right mount point for `/boot/efi` so that the kernel will be installed in the right place.

## Run `guix system init`

Finally we are going to build our system onto our target disk! Make sure that the target system is mounted at `/mnt` and here we go!

```bash
# guix pull
# guix system init config.scm /mnt
```

Depending up on the time between building the USB key image and doing the pull, this will time to some time to build. (The main thing is if the linux kernel version is different.) This builds all of the dependant packages from our `operatating-system` definition, and then moves overything over to the filesystem in `/mnt` and should update the grub bootloader.

Once this is done you should be able to reboot, remove the USB key, and them boot into your new Guix System installation!

## Setup

Once the system is booted, do `C-Alt-F2` to get a console, and log in as root. Then give root and your username a password using `passwd`.

Turn the wifi on using `rfkill unblock all`.

You may want to put the `nonguix` channel back into `/root/.config/guix/channels.scm` to help with changing things in the future.

Enjoy!

## References

1. https://www.gnu.org/software/guix/download/
2. https://www.gnu.org/software/guix/manual/en/html_node/Binary-Installation.html#Binary-Installation
3. https://www.gnu.org/software/guix/manual/en/html_node/Installing-Guix-in-a-VM.html#Installing-Guix-in-a-VM
4. https://gitlab.com/nonguix/nonguix/blob/master/README.org
5. https://forums.intel.com/s/question/0D50P0000490X0zSAE/image-authorization-fail-system-can-not-boot-to-this-usb-device?language=en_US
6. https://lists.gnu.org/archive/html/help-guix/2016-02/msg00046.html
7. https://bbs.archlinux.org/viewtopic.php?pid=1324810#p1324810
8. https://libreboot.org/docs/gnulinux/guix_system.html
