---
title: "Getting started with qemu"
date: 2018-09-10
origin: 
alternate: https://drewdevault.com/2018/09/10/Getting-started-with-qemu.html
type: link
---

<p>I often get asked questions about using my software, particularly sway, on
hypervisors like VirtualBox and VMWare, as well as for general advice on
which hypervisor to choose. My answer is always the same: qemu. There’s no
excuse to use anything other than qemu, in my books. But I can admit that it
might be a bit obtuse to understand at first. qemu’s greatest strength is also
its greatest weakness: it has so many options that it’s hard to know which ones
you need just to get started.</p>
<p>qemu is the swiss army knife of virtualisation, much like ffmpeg is the swiss
army knife of multimedia (which comes as no surprise, given that both are written
by Fabrice Bellard). I run a dozen permanent VMs with qemu, as well as all of
the ephemeral VMs used on <a href="https://meta.sr.ht">builds.sr.ht</a>. Why is it better
than all of the other options? Well, in short: qemu is fast, portable, better
supported by guests, and has more features than Hollywood. There’s nothing other
hypervisors can do that qemu can’t, and there’s plenty qemu can that they
cannot.</p>
<p>Studying the full breadth of qemu’s featureset is something you can do over
time. For now, let’s break down a simple Linux guest installation. We’ll start
by downloading some install media (how about <a href="https://alpinelinux.org/">Alpine
Linux</a>, I like Alpine Linux) and preparing a virtual
hard drive.</p>
<div><div><pre><code>curl -O https://nl.alpinelinux.org/alpine/v3.8/releases/x86_64/alpine-standard-3.8.0-x86_64.iso
qemu-img create -f qcow2 alpine.qcow2 16G
</code></pre></div></div>
<p>This makes a 16G virtual hard disk in a file named alpine.qcow2, the qcow2
format being a format which appears to be 16G to the guest (VM), but only
actually writes to the host any sectors which were written to by the guest in
practice. You can also expose this as a block device on your local system (or a
remote system!) with qemu-nbd if you need to. Now let’s boot up a VM using our
install media and virtual hard disk:</p>
<div><div><pre><code>qemu-system-x86_64 \
    -enable-kvm \
    -m 2048 \
    -nic user,model=virtio \
    -drive file=alpine.qcow2,media=disk,if=virtio \
    -cdrom alpine-standard-3.8.0-x86_64.iso \
    -sdl
</code></pre></div></div>
<p>This is a lot to take in. Let’s break it down:</p>
<p><strong>-enable-kvm</strong>: This enables use of the KVM (kernel virtual machine) subsystem
to use hardware accelerated virtualisation on Linux hosts.</p>
<p><strong>-m 2048</strong>: This specifies 2048M (2G) of RAM to provide to the guest.</p>
<p><strong>-nic user,model=virtio</strong>: Adds a virtual <strong>n</strong>etwork <strong>i</strong>nterface
<strong>c</strong>ontroller, using a virtual LAN emulated by qemu. This is the most
straightforward way to get internet in a guest, but there are other options (for
example, you will probably want to use <code>-nic tap</code> if you want the guest to do
networking directly on the host NIC). <code>model=virtio</code> specifies a special
<code>virtio</code> NIC model, which is used by the virtio kernel module in the guest to
provide faster networking.</p>
<p><strong>-drive file=alpine.qcow2,media=disk,if=virtio</strong>: This attaches our virtual
disk to the guest. It’ll show up as <code>/dev/vda</code>. We specify <code>if=virtio</code> for the
same reason we did for <code>-nic</code>: it’s the fastest interface, but requires special
guest support from the Linux virtio kernel module.</p>
<p><strong>-cdrom alpine-standard-3.8.0-x86_64.iso</strong> connects a virtual CD drive to the
guest and loads our install media into it.</p>
<p><strong>-sdl</strong> finally specifies the graphical configuration. We’re using the SDL
backend, which is the simplest usable graphical backend. It attaches a display
to the guest and shows it in an <a href="https://www.libsdl.org/">SDL</a> window on the
host.</p>
<p>When you run this command, the SDL window will appear and Alpine will boot! You
can complete the Alpine installation normally, using <code>setup-alpine</code> to install
it to the attached disk. When you shut down Alpine, run qemu again without
<code>-cdrom</code> to start the VM.</p>
<p>That covers enough to get you off of VirtualBox or whatever other bad hypervisor
you’re using. What else is possible with qemu? Here’s a short list of common
stuff you can look into:</p>
<ul>
<li>Running pretty much any guest operating system</li>
<li>Software emulation of non-native architectures like ARM, PPC, RISC-V</li>
<li>Using <code>-spice</code> instead of <code>-sdl</code> to enable remote access to the
display/keyboard/mouse</li>
<li>Read-only disk images with guest writes stored in RAM (<code>snapshot=on</code>)</li>
<li>Non-graphical boot with <code>-nographic</code> and <code>console=ttyS0</code> configured in your
kernel command line</li>
<li>Giving a genuine graphics card to your guest with KVM passthrough for high
performance gaming, OpenCL, etc</li>
<li>Using <a href="https://virt-manager.org/">virt-manager</a> or
<a href="https://help.gnome.org/users/gnome-boxes/stable/">Boxes</a> if you want a GUI to
hold your hand</li>
<li>And much more…</li>
</ul>
<p>There’s really no excuse to be using any other hypervisor<sup><a href="https://drewdevault.com/2018/09/10/Getting-started-with-qemu.html#fn:1">1</a></sup>. They’re all
dogshit compared to qemu.</p>
<div>
<ol>
<li>
<p>Especially VirtualBox. If you use VirtualBox after reading this article you make poor life choices and are an embarrassment to us all. <a href="https://drewdevault.com/2018/09/10/Getting-started-with-qemu.html#fnref:1">↩</a></p>
</li>
</ol>
</div>