---
title: "Arch Linux won't boot, now what?"
description: "Fixing a broken Arch Linux system"
keywords: Arch,Linux,boot,GRUB,chroot,arch-chroot,pacman,Clonezilla,disk
---

I've been using Arch Linux for the last 5 years and I'm very happy with its 
simplicity. It's great to be able to install **only** the packages that you 
actually need and get their the most up-to-date versions. Arch Linux is 
actually really stable, but once a year or so my system breaks. This usually
happens after updating the system or messing with configuration files.
As this sort of incident becomes more rare I tend to forget what to do to fix my 
broken system, so I decided to write here the common steps that I take in these 
situations. 

<!--more-->

## Keep system backups

Before doing anything, make sure you keep system backups. I think I can't 
stress enough how important it is to have full backups of your hard drive. 
I don't usually have to restore from backup but it's good to be prepared for 
worst case scenario. Please bear in mind that disks can and will fail at some 
point. Don't lose your data.

I try to backup my hard drive before a system update at least once every 3 
months. I've been keeping in my desk a USB flash drive with [Clonezilla](
https://clonezilla.org/) installed for years. This makes it super easy to clone
the entire disk to an image in an external hard drive. I just boot Clonezilla 
from the USB drive and follow the "beginner" steps to clone my disk. When it
comes to restoring the disk from a saved image, the process is just as simple 
and straight-forward.

## Boot Arch from USB flash drive

When your system breaks and you can't log in, the first thing you have to do
is [install Arch Linux on a USB flash drive](
https://wiki.archlinux.org/index.php/Installing_Arch_Linux_on_a_USB_key) and 
use it as a rescue USB. This gives you a terminal that you can use to access
your system. You are likely to spend a good amount with this bare bones terminal
so I suggest to take a few minutes to tweak it a little to make it more 
comfortable for you. In my case, my keyboard has a Spanish layout so I set the
keyboard layout accordingly:

```
loadkeys es
```

## Mount the Linux filesystem

From the live installation, You can access your files and data by mounting
the Linux filesystem partition.

```
mount /dev/sdaX /mnt
```

`/dev/sdaX` has to be replaced with the actual partition. You can check that 
by running `fdisk -l`. In my case, this is the output:

```
Device       Start        End    Sectors  Size Type
/dev/sda1     2048     196607     194560   95M EFI System
/dev/sda2   196608    8007679    7811072  3.7G Linux swap
/dev/sda3  8007680 3907028991 3899021312  1.8T Linux filesystem
```

As you can see `/dev/sda1` is the "boot" partition, `/dev/sda2` is used for 
swap memory and `/dev/sda3` is the one that I have to mount.

Mounting the filesystem by itself is not very useful. To actually use the 
files and programs installed in that partition you have to log in as a known
user, or ["chroot"](https://wiki.archlinux.org/index.php/Chroot) into the system.

## chroot

Now that the filesystem is mounted at `/mnt` (or wherever you want), you can 
chroot like this:

```
arch-chroot /mnt
```

Now you have root access to your system, you are free to do anything you want.
Before proceeding it would be wise to mount the boot partition at `/boot` 
because the root of the issue might be there. 

```
mount /dev/sdaX /boot
```

Once again `/dev/sdaX` has to be replaced with the actual boot partition 
as listed by `fdisk -l`.

As root user, here are some things that I suggest doing:

#### Check recently edited config files

Did you recently edit configuration files manually for your bootloader, window 
manager or desktop environment? You definitely want to check those and try to 
revert any changes that could have gone wrong. It's a good idea to keep a backup
copy of a configuration file before editing, specially if you don't really know
what you are doing.

#### Check your kernel

If your system fails to boot with an error message `Error loading \vmlinuz-linux: 
not found` or similar, you might want to check that your kernel is installed 
correctly. Run `pacman -Q linux` and `uname -r`, they should have the same kernel 
version. If they don't, or you are just feeling paranoid, you can reinstall it
with `pacman -S linux`.


#### Rebuild the initramfs image

It might be a good idea to rebuild the initial ram disk environment, specially
if you reinstalled the kernel, although technically pacman hooks should take 
care of this. Make sure your boot partition is mounted at `/boot`, otherwise 
none of this may work, then just run this command:

```
mkinitcpio -P
```


#### Reinstall the bootloader

Reinstalling the bootloader or rebuilding its configuration files can also
help fixing the system. This may be necessary after doing changes inside the 
boot partition, like when rebuilding the initramfs image. 

In my case I use [GRUB](https://wiki.archlinux.org/index.php/GRUB), so 
reinstalling it is fairly easy. Again, make sure the boot partition is mounted 
at `/boot`, then run these two commands:

```
grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=GRUB
grub-mkconfig -o /boot/grub/grub.cfg
```

#### Restore all packages to a specific date

Probably the most likely reason for Arch breaking is a system update 
(`pacman -Syu`). Sometimes new package versions introduce breaking changes that 
are  incompatible with the rest of the system. There's not much you can do here, 
other than helping out in the [bugtracker](https://bugs.archlinux.org/) and 
waiting for the issue to get sorted out. In the mean time, you can revert all 
your packages to a specific date in the past using the 
[Arch Linux Archive](https://wiki.archlinux.org/index.php/Arch_Linux_Archive),
a server that stores official repositories snapshots and provides URLs to
retrieve them easily. If you don't know what date you should revert to, 
you can check pacman logs at `/var/log/pacman.log`. In my case, my last 
successful update was on September 30, so I'll use that. 

First you have to edit `etc/pacman.conf` and set the Arch Linux Archive URL
for every repository. Here's how my config file looks normally:

```
[core]
Include = /etc/pacman.d/mirrorlist

[extra]
Include = /etc/pacman.d/mirrorlist

[community]
Include = /etc/pacman.d/mirrorlist

[multilib]
Include = /etc/pacman.d/mirrorlist
```

To rollback, instead of including the mirrorlist, set the snapshot URL like 
this:

```
[core]
SigLevel = PackageRequired
Server=https://archive.archlinux.org/repos/2020/09/30/$repo/os/$arch

[extra]
SigLevel = PackageRequired
Server=https://archive.archlinux.org/repos/2020/09/30/$repo/os/$arch

[community]
SigLevel = PackageRequired
Server=https://archive.archlinux.org/repos/2020/09/30/$repo/os/$arch

[multilib]
SigLevel = PackageRequired
Server=https://archive.archlinux.org/repos/2020/09/30/$repo/os/$arch
```

Finally, run `pacman -Syyuu` to update the database with the archived packages.
This might take a while, specially if you depend on close mirrors for fast 
downloads, but once it's done your system should be working as it did before. 
You are now effectively stuck in the past, though. Once you are sure that 
there are no more broken packages in the official repositories you can revert 
the `/etc/pacman.conf` changes and update your system again.
