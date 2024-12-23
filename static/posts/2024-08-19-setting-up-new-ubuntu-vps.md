---
title: "Setting up a new Ubuntu VPS"
description: "Things to do when setting up a new VPS running Ubuntu"
keywords: Ubuntu,Linux,server,VPS,nginx,ufw,vim
---

I've been doing a lot of backend development this past few months and all my 
deployments so far have been with [Digital Ocean droplets](
https://www.digitalocean.com/products/droplets) running Ubuntu Linux. I think
it's pretty cool how much you can do with a Linux box in the cloud for
so cheap, so I decided to write a little guide with all of the things that I 
might want/have to do every time I create a new droplet.

<!--more-->

### Configure Vim

When setting up a new VPS we'll need to edit a few configuration files. Vim is 
my GOTO editor on any computer, and Ubuntu does have it installed right out of
the box, but Vim's default config is pretty terrible. I keep my dotfiles in 
[a GitHub repo](https://github.com/GAumala/dotfiles) so I can quickly download
them to any system, desktop or server.

However, when it comes to production servers I would rather use a minimal config 
with no plugins to avoid any security risks with third party code. I usually just
copy this: 

``` vimscript
syntax on

" Use spaces instead of tabs
set tabstop=2       " The width of a TAB is set to 2.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 2.

set shiftwidth=2    " Indents will have a width of 2

set softtabstop=2   " Sets the number of columns for a TAB

set expandtab       " Expand TABs to spaces

" escape ESC
imap kj <Esc>

"split new buffers to right
set splitright

" numbers column
set nu
set relativenumber

filetype plugin on

" reload buffers from disk when they are updated externally
set autoread

" http://vim.wikia.com/wiki/Recover_from_accidental_Ctrl-U
inoremap <c-w> <c-g>u<c-w>
inoremap <c-u> <c-g>u<c-u>

" automatically change working dir to active buffer's dir
set autochdir

" don't show matching parenthesis
let g:loaded_matchparen=1
```

This should go in `/root/.vimrc`. If you are creating more users, 
you should create another `.vimrc` on their home directory.

### Add your VPS IP address to your local /etc/hosts

If you are not planning to assign a public domain to your VPS, 
you can just make one up in your local machine's `/etc/hosts`. If you're going
to frequently SSH into your VPS, it's better to do `ssh root@my-new-vps.com`
than `ssh root@107.168.0.11`. Just add this line to `/etc/hosts`:

```
107.168.0.11 my-new-vps.com
```

### Create a new user

If you plan to run your own software on a VPS, you should really consider
doing it with a less privileged user. Any security flaw in your code could
compromise your entire server if it's running as root. To create a new
user "gabriel" just run:

``` bash
useradd -m -s /bin/bash gabriel
```

I add the `-s` flag to specify `bash` because the default is `sh`. The `-m` flag
just creates the user's home directory `/home/gabriel`. 

For users in server machines, I prefer to not use passwords. You can use the 
`su` command to switch between users, or just login directly into that user
via SSH. For the later to work your new user needs a copy of your local 
machine's public SSH key in `$HOME/.ssh/authorized_keys`. You can also just copy the
contents of `/root/.ssh/authorized_keys` but make sure the new user can read it!

### Setup Git repositories

Git lets you use SSH urls for remote repositories, so you can push your code 
into your VPS. I use this frecuently to backup my private repositories. Git is
already installed in Ubuntu, so to do this just create a directory and
initialize a bare repository in it:

``` bash
su -l gabriel
mkdir MyProject.git
git init --bare MyProject.git
```

Bare repositories only have the default branch, which is `master`. If you
attempt to push to any other branch it will fail. This is a common issue
because nowadays most repositories use `main` as the default branch. 
A good solution for this is to change the default branch name to `main`
**before** creating any repositories:

``` bash
git config --global init.defaultBranch main
```

To push into the new `MyProject.git` directory, the url would be:

```
ssh://gabriel@my-vps.com:/home/gabriel/MyProject.git
```

Don't forget that `MyProject.git` must be writeable for the user that will
SSH into your VPS every time you want to `git push`. This is the reason
why I used `su -l gabriel` before creating the repository. 

Please note that `MyProject.git` is not the root of the repository. If you
want to browse the repository files you must checkout those files into an
existing directory like this:

```
git --work-dir=/home/gabriel/MyProject --git-dir=/home/gabriel/MyProject.git checkout -f
```

This is useful for deploying web applications to your VPS. For most projects
it's astronomically faster to just push source files with git than uploading
huge binary files. 

### Setup OpenVPN

You can use your VPS as a VPN if you install OpenVPN. This is particularly handy
for me when I'm at airports using public wi-fi. I don't recommend using this for
Netflix or any other streaming service because your VPS probably has a limited
bandwith. 

The easiest way to install and setup OpenVPN is with [openvpn-install](
https://github.com/angristan/openvpn-install) . Just log in as root and run:

``` bash
curl -O https://raw.githubusercontent.com/angristan/openvpn-install/master/openvpn-install.sh
bash ./openvpn-install.sh
```

After a few prompts, the script will output a `.ovpn` file that you can download
 to your local machine. Your OpenVPN client should have an option to let you import
configuration from a `.ovpn` file.

### Setup Nginx with Let's Encrypt

If you are planning to deploy a web application with HTTPS, you can get
a certificate for free with Let's Encrypt. Setting this up wth Nginx is super
easy, but it takes a considerable number of steps that would need their own
blog post. I always use [this neat guide from Digital Ocean.](
https://www.digitalocean.com/community/tutorials/how-to-secure-nginx-with-let-s-encrypt-on-ubuntu-22-04
)

### Setup a Firewall

Once you have all your applications ready to run in your VPS, you should
setup a firewall for security. By default Ubuntu has `ufw` running but
it is inactive. You have to configure it using the command line. 

The first thing you should do is deny all incoming traffic as a default policy:

``` bash
ufw default deny incoming
```

Then you should allow all applications and ports that you use. You can run 
`ufw app list` to check available apps in your system. The most  important one
is OpenSSH. You must allow it, otherwise you'll be locked out of your VPS.

``` bash
ufw allow OpenSSH
```

You can also allow specific ports that you use. If you have an app listening
on port 8080, run:

``` bash
ufw allow 8080
```

Sometimes you might want to allow incoming traffc only for a specific IP address.
For example, if I'm using this VPS to deploy a Postgres database (port 5432),
I would like the firewall to only allow my application server (address
107.168.0.11) to access it. To do that, just run:

``` bash
ufw allow from 107.168.0.11 to any port 22
```

Finally enable your firewall with:

```
ufw enable
```

After enabling, you can check your current configuration with:

```
ufw status verbose
```
