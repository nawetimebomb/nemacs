                                  ___           ___           ___           ___           ___           ___
                                 /\__\         /\  \         /\__\         /\  \         /\  \         /\  \
                                /::|  |       /::\  \       /::|  |       /::\  \       /::\  \       /::\  \
                               /:|:|  |      /:/\:\  \     /:|:|  |      /:/\:\  \     /:/\:\  \     /:/\ \  \
                              /:/|:|  |__   /::\~\:\  \   /:/|:|__|__   /::\~\:\  \   /:/  \:\  \   _\:\~\ \  \
                             /:/ |:| /\__\ /:/\:\ \:\__\ /:/ |::::\__\ /:/\:\ \:\__\ /:/__/ \:\__\ /\ \:\ \ \__\
                             \/__|:|/:/  / \:\~\:\ \/__/ \/__/~~/:/  / \/__\:\/:/  / \:\  \  \/__/ \:\ \:\ \/__/
                                 |:/:/  /   \:\ \:\__\         /:/  /       \::/  /   \:\  \        \:\ \:\__\
                                 |::/  /     \:\ \/__/        /:/  /        /:/  /     \:\  \        \:\/:/  /
                                 /:/  /       \:\__\         /:/  /        /:/  /       \:\__\        \::/  /
                                 \/__/         \/__/         \/__/         \/__/         \/__/         \/__/

# About me

> I am an enthusiastic Software Engineer who loves to work in a team, sharing knowledge and experience among peers.

I started programming when I was 12 years old. I've been working on a few open source projects as well as doing
freelance. At the age of 23, I started a professional career in Web Frontend, though I still prefer more logical stuff
instead of User Interface.

# Features

- "Stock Emacs".
- Descritive [configuration](nemacs.org).
- Always up-to-date.
- Fixes the =C-a= to go to indentation if you press twice.
- A monochromatic theme.
- Not disruptive when programming, you do all the work.
- Better defaults are better.
- Support Terminal and Window System, as well as multiple OS.
- Designed to work nicely with `emacs --daemon`.
- Simple and easy to understand mode-line.

# Download This Configuration

My Emacs configuration has some personal touch even though lately I use mostly a "stock Emacs". I use ~~Terminal Emacs~~
default Emacs and sometimes Terminal Emacs, so most of the colors are adapted to how my terminals render Emacs.

## Download Emacs in Mac

```
$ brew tap d12frosted/emacs-plus
$ brew install emacs-plus --without-spacemacs-icon
```

## Download Emacs in Ubuntu (old version)

```
$ [sudo] add-apt-repository ppa:kelleyk/emacs
$ [sudo] apt-get update
$ [sudo] apt-get install emacs25
```

## Download Emacs in ArchLinux (where I mostly use Emacs)

```
[sudo] pacman -S emacs
```

## Download Emacs in Windows

> The reason for GNU Emacs's existence is to provide a powerful editor for the GNU operating system. Versions of GNU,
> such as GNU/Linux, are the primary platforms for Emacs development.
> However, GNU Emacs includes support for some other systems that volunteers choose to support.

Not specific instructions here. I've tried a couple of different installations and I had a couple of problems, mostly
because I cannot find a consistent way to install Emacs in Windows since sometimes I'm behind a firewall/vpn security.

I'm using chocolatey Emacs right now but I heard `mingw` version is the best one. If you're going for that, then:

```
pacman -S mingw-w64-x86_64-emacs
```

# Contributions

If you see something that can be done in a better way or some configuration that you consider it's missed go ahead and
submit a PR.
