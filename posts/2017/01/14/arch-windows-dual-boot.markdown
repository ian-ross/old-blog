---
author: Ian
title: Setting up dual boot Arch Linux/Windows 10
published: 2017-01-14 17:55:23
---

I had to do a slightly weird bit of system admin recently, and there
was one step that was kind of sneaky and not something I'd seen
mentioned anywhere else.  So I thought I'd better write it up...

What I wanted to do was make my desktop machine dual boot Arch Linux
and Windows 10 (I needed Windows to use some CAD software).  That's
not so unusual, but the recommended procedure seems to be to start
from a clean machine, install Windows, then install Linux.  I was
starting from a pre-existing Arch Linux installation and didn't want
to lose anything.  I also had things set up using `syslinux` in BIOS
mode, which wasn't going to work with Windows.  So I needed to switch
to UEFI booting first.

<!--MORE-->

Here's what I did:

#### 0. Check backups!

That's always going to be the first step if you're about to do
something potentially destructive to a machine, but it's easy to
forget.  I use [SpiderOak ONE][spideroak] for backups, so it was just
a case of making sure that things hadn't mysteriously stopped working
for some reason and that backups were reasonably up-to-date.

#### 1. Make GParted Live USB

The first thing I needed to do was move partitions around.  Doing this
for the disk containing your root partition used to be a bit of a
hassle, but no longer!  Just make a [GParted Live USB][gparted], boot
from it and edit partitions on any disk as needed.  And GParted is a
*really* good partition editing tool: it can resize and move
partitions without any drama, which also used to be a real hassle (or
impossible!).

#### 2. Move partitions around

I have an SSD and a "normal" hard disk in my machine, and I decided to
make a partition on the spinning disk for Windows.  Apparently Windows
10 is happier being on a partition at the beginning of a disk, so I
moved and resized the existing single big partition on this disk to
put it at end and make enough space at the beginning for Windows.
Then, for the UEFI setup, you need to make an EFI System Partition
(ESP) somewhere.  I put this on the SSD, resizing the big `/home`
partition on that disk to make space.  (You only need 512 Mb for the
ESP.)  I also made another partition for sharing data between Linux
and Windows.

#### 3. Set up ESP

The ESP needs to be a FAT32 partition with the boot flag set.  You can
set all that up in Gparted.

#### 4. Install GRUB and `efibootmgr`

Now you boot back into Linux to start setting up for UEFI boot.  You
need to install the GRUB bootloader and the `efibootmgr` package (for
setting UEFI variables, among other things).

#### 5. Set up GRUB

To do this, just mount the ESP at `/efi-boot` and do

```
grub-install --target=x86_64-efi --efi-directory=/efi-boot
   --bootloader-id=grub --boot-directory=/efi-boot --debug
grub-mkconfig -o /efi-boot/grub/grub.cfg
```

Unfortunately, that first step doesn't quite work!  That's because the
machine is booted in BIOS mode, and GRUB needs to set some EFI
variables to make the UEFI boot work.  But there's no EFI stuff around
to allow it to do that.

#### 6. The sneaky step

Well, it might not seem so sneaky after the fact, but I was quite
pleased to work out how to do this, and I've not seen this documented
anywhere else, so for what it's worth, here's what you do.  Make a
bootable Arch Linux USB (set up for UEFI boot!) and boot from it.
You'll also need to enable UEFI boot in your BIOS (that's kind of
obvious, but it will be confusing if you don't do it).  Boot from the
USB: you should see the USB stick show up as a UEFI boot option in the
boot menu -- select that one.  Now, mount the ESP at `/efi-boot` and
rerun the `grub-install` command from step 5.  This time, you're in
UEFI mode, so GRUB is able to set the EFI variables it needs to, and
since everything else in the GRUB setup is the same between booting
"for real" and booting from the USB, everything works fine.

#### 7. UEFI boot!

Now if you reboot, your machine should boot using UEFI and you should
see the GRUB bootloader menu.

#### 8. Install Windows 10

At this point, you should be able to install Windows 10 without too
much drama: make a USB from the ISO, boot from it, choose a custom
install, and pick the right partition to put Windows on.  If all goes
well, the installation will work without trouble, and you'll still be
able to boot Linux afterwards.

#### 9. Boot Windows from GRUB

Finally, you just need to add a GRUB menu entry to chainload Windows:
you add an entry to `/etc/grub.d/40_custom`, then rerun
`grub-mkconfig`.  (There are plenty of examples of how to do this step
around the web.)

I don't know if that will be useful to anyone else at all, but it went
surprisingly smoothly once I'd figured out "the sneaky step".

[spideroak]: https://spideroak.com/
[gparted]: http://gparted.sourceforge.net/livecd.php
