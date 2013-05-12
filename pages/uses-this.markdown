---
layout: page
title: Uses This
published: 2013-04-21
comments: true
footer: true
---

* toc

This page is setup in the spirit of the website [of the same name](http://usesthis.com/). In my [about page](/about) I vaguely hint, in general terms, of the tools I like to use. I do this so that my about page doesn't get littered with what might end up being fads and hipster tools. Instead this page is here to list the specific tools I use at any given moment.

### Hardware

Most of my life I've had pretty average hardware but I decided to indulge myself with my current system. This is also my first desktop with an Intel Processor.

Core:

* Intel Core i7 2600k @ 4.2 GHz with Cooler Master Hyper 212 Plus
* Asus P8Z68-V Pro
* G.Skill RipJaws Series 4 x 4 GB DDR3 1600
* EVGA GeForce GTX 460 SuperClocked EE 1 GB
* 3 SSDs (120 GB Windows, 60 GB Mac OS X, 60 GB Arch Linux)
* 3 HDDs (2 x 1 TB, 1.5 TB)

Peripherals:

* Dell UltraSharp 23" 1080p IPS
* [Das Keyboard Model S Ultimate](http://www.daskeyboard.com/model-s-ultimate/) (Mechanical)
* Logitech G500 Laser Mouse
* Cooler Master CM 690 II Advanced
* [Zalman Zm-Mic1 Microphone](http://amzn.com/B00029MTMQ)
* [Shure SE215-K](http://amzn.com/B004PNZFZ8)

### Operating System

Exemplifying my ideology of non-fanaticism, I have Windows 8, Arch Linux, and Mac OS X 10.8 Mountain Lion installed on my computer. This [picture](http://i.imgur.com/Xa5suXo.jpg) shows the custom boot screen I designed which I'm greeted with upon booting. This is not just for show; I actually use each one regularly for different things.

The setup consists of SSDs for the OSes and regular HDDs for media. Windows is on a 120 GB SSD and Mac and Linux are on separate 60 GB SSDs. Aside from this I have two 1 TB and one 1.5 TB HDDs for media storage.

#### Windows

Windows is my casual-use operating system which I boot into when I want to watch movies, play games, or just browse. Because of this, my media drives consisting of movies, music, etc. are formatted in NTFS. I also use it to sync my iPhone with iTunes.

#### Linux

When I need to get work done, I usually boot into Linux. The distribution I have installed is [Arch Linux](http://www.archlinux.org/). This is so far my favorite distribution out of the many I've used over the years. It offers a compromise between [LFS](http://www.linuxfromscratch.org/index.html)/[Gentoo](http://www.gentoo.org/) level of flexibility and leanness with very good package management consisting of bleeding-edge packages.

I've noticed that some people seem to treat it as the "elite" distribution that "all the cool people" use. It's touted as being complicated to install and use due to its lean state out-of-the-box, the implication being that anyone who installs it must be some sort of genius, completely disregarding the fact that most people simply follow the step-by-step instructions in the wiki -- which anyone is capable of following. [This comment](http://www.reddit.com/r/linux/comments/qa4ia/linus_there_is_no_way_in_hell_i_can_honestly/c3w0v2p) sums up my opinion about this reputation of Arch. I don't participate in this kind of fanaticism (nor do I believe it's true) -- it's simply the distribution I've come to genuinely like.

The DE/WM I use depends on the level of work needed to be done. My default DE is [GNOME](http://www.gnome.org/), but if I have a medium load of work I use [Awesome](http://awesome.naquadah.org/), and [xmonad](http://xmonad.org/) + [dzen2](https://sites.google.com/site/gotmor/dzen) + [dmenu](http://tools.suckless.org/dmenu/) if I have a lot of work. I am very interested in and regularly read about the POSIX standard ([SUS](http://en.wikipedia.org/wiki/Single_UNIX_Specification)) and the Linux Userspace Interface.

#### Mac OS X

Mac OS X used to be my general development OS because I really liked OS X's font rendering, monospace fonts, and UI aesthetic, but that position has been ceded to Linux for its flexibility and for the various applications becoming available on Linux, mainly Netflix, Air Video Server, myihome, Steam, and Spotify. Now OS X mainly stays around in case I'd like to do some iOS development and to make sure my programs and websites work fine on OS X.

### Editor

I used to think that people who boasted about using [vim](http://en.wikipedia.org/wiki/Vim_(text_editor)) were just trying to appear to be elite that they claim to work with a seemingly archaic terminal-based editor. However, a friend who first showed me his use of vim through an ssh connection many years ago, when I was barely starting out with software development, managed to show me the level of fluent mastery of text editing that could be achieved with the editor. A few years ago I dedicated time to really learning its vocabulary and customization. The result is that I feel to be in complete control over the text which I am manipulating. That said, I don't engage in petty text editor wars nor do I attempt to push it on others. I simply use vim to my benefit.

#### Looks

The truth is that the default configuration of vim is pretty ugly, and so are most of the themes it comes with. This shouldn't really matter, as its benefits greatly outweigh any cosmetic qualities, but it does matter to me. For this reason I designed [my own syntax highlighting color scheme](https://github.com/blaenk/dots/blob/master/vim/vim.ln/colors/blaenk.vim) (and a [light version](https://github.com/blaenk/dots/blob/master/vim/vim.ln/colors/blaenklight.vim) which is used on this site). I primarily use gui versions of vim such as gvim or [MacVim](https://code.google.com/p/macvim/) for their added benefits (e.g. wider range of bindable keys, more colors, etc.). My configuration files [hide the menus and toolbars](https://github.com/blaenk/dots/blob/master/vim/gvimrc.ln#L2-L4) so that only the tab bar shows, making for pretty compact windows which are perfect for tiling window managers.

#### Plugins

I use [many plugins](https://github.com/blaenk/dots/blob/master/vim/vim.ln/conf/bundles.vim). I believe the most noteworthy are [vundle](https://github.com/gmarik/vundle) for plugin management, [ctrlp](http://kien.github.io/ctrlp.vim/) for fuzzy file opening/switching, [powerline](https://github.com/Lokaltog/vim-powerline) for a beautiful and functional status line, [surround](https://github.com/tpope/vim-surround) to easily manipulate text's surrounding delimiters, and [YouCompleteMe](http://valloric.github.io/YouCompleteMe/) for clang-powered auto-complete.

### Development Tools

My favorite source control system so far is [git](http://git-scm.com/). I regularly spend time learning about its internals.

My compiler of choice is [clang](http://clang.llvm.org/) paired with [GDB](http://www.gnu.org/software/gdb/) (until [LLDB](http://lldb.llvm.org/) matures on Linux).

### Miscellaneous

[Kindle](http://amzn.com/B007HCCNJU) for reading books. At first I only used it to read novels and other casual material, but eventually I began using it for more and more technical books, starting with C++ Primer 5th Ed. (and the GDB book and TLPI). I actually used to explicitly prefer physical books, now I consistently find that I can read a lot quicker and have more fun doing it on a kindle. 

[Chrome](https://www.google.com/intl/en/chrome/browser/) is my primary browser (Chromium on Linux).

I use [Syncplay](http://syncplay.pl/) regularly to watch movies with friends, in sync.

[MPC-HC](http://mpc-hc.org/) is my primary -- and favorite by _far_ (so much that I've contributed to it) -- media player on Windows. [mplayer2](http://www.mplayer2.org/) on Linux. Both work with Syncplay.

I mainly use [mumble](http://mumble.info/) for voice communication.

*[DE]: Desktop Environment
*[WM]: Window Manager
