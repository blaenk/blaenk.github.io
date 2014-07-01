---
title: Uses This
published: 2013-04-21
comments: off
---

* toc

This page is setup in the spirit of the website [of the same name]. This page is here to list the specific tools I use at any given moment.

[of the same name]: http://usesthis.com/

## Site

This site is built with [Hakyll], the static site generator written in Haskell. The site and its source code are hosted [on Github]. The site utilizes [Pandoc] for converting Markdown (and any of the various other formats Pandoc supports), [SCSS] for processing stylesheets, [Pygments] for syntax highlighting, and [MathJax] for math notation.

[Hakyll]: http://jaspervdj.be/hakyll/
[on Github]: https://github.com/blaenk/blaenk.github.io
[Pandoc]: http://johnmacfarlane.net/pandoc/
[SCSS]: http://sass-lang.com/
[Pygments]: http://pygments.org/
[MathJax]: http://www.mathjax.org/

It is [heavily modified] from plain Hakyll, although that's more or less the point. Hakyll is more of a library for static site generation which one employs how they see fit, rather than a framework that is worked under. I created various Pandoc [abstract syntax tree] (AST) transformers to implement features such as table of contents generation and pygments syntax highlighting integration. I also implemented a Haskell compiler for automatic abbreviation substitution. Perhaps best of all, I also [implemented live editing], where my changes show up instantly in my browser as soon as I save the file---which has completely changed the way I write content for this site. This is accomplished via Haskell STM Channels piped to HTML5 WebSockets.

[heavily modified]: /tags/hakyll/
[abstract syntax tree]: http://en.wikipedia.org/wiki/Abstract_syntax_tree
[implemented live editing]: /posts/live-editing-with-hakyll/

I originally started this site with [Jekyll], but I had been wanting to switch to Hakyll so that I could keep my knowledge of Haskell fresh. This seemed like the perfect project to use it for. I feel that Haskell maps to the problem very appropriately, especially with amazing tools like Pandoc and Hakyll. The fact that Haskell is compiled also makes for a very appreciable difference in speed.

[Jekyll]: http://jekyllrb.com/

I publish a lot of my [notes] to this site.

[notes]: /notes/

## Operating System

Exemplifying my belief of non-fanaticism, I have Windows 8, Arch Linux, and Mac OS X 10.8 Mountain Lion installed on my computer. This [picture] shows the custom boot screen I designed which I'm greeted with upon booting. This is not just for show; I actually use each one regularly for different things.

[picture]: http://i.imgur.com/Xa5suXo.jpg

The setup consists of SSDs for the OSes and regular HDDs for media. Windows is on a 120 GB SSD and Mac and Linux are on separate 60 GB SSDs. Aside from this I have two 1 TB and one 1.5 TB HDDs for media storage.

### Windows

I go into Windows when friends want to play Windows-only games. I also use it to sync my iPhone with iTunes.

### Linux

This is my primary OS. The distribution I have installed is [arch linux], which is my favorite distribution out of the many I've used over the years. It offers a compromise between [LFS]/[Gentoo] level of flexibility and leanness with very good package management consisting of bleeding-edge packages.

[arch linux]: http://www.archlinux.org/
[LFS]: http://www.linuxfromscratch.org/index.html
[Gentoo]: http://www.gentoo.org/

My preferred desktop environment is [gnome]; I don't feel the need to posture with obscure tiling managers---though I genuinely do enjoy using [xmonad]---I prefer the experience that Gnome provides. I use [aura] as my [pacman] wrapper for seamless integration with the [AUR]. I am very interested in and regularly read about the POSIX standard ([SUS]) and the Linux Userspace Interface.

[gnome]: http://en.wikipedia.org/wiki/GNOME
[xmonad]: http://xmonad.org/
[aura]: https://github.com/fosskers/aura
[pacman]: https://wiki.archlinux.org/index.php/Pacman
[AUR]: https://wiki.archlinux.org/index.php/Arch_User_Repository
[SUS]: http://en.wikipedia.org/wiki/Single_UNIX_Specification

I use [zsh] as my shell and [urxvt] as my terminal. More information on my configuration is available in my [terminal customization] post.

[zsh]: http://en.wikipedia.org/wiki/Z_shell
[urxvt]: http://en.wikipedia.org/wiki/Rxvt-unicode
[terminal customization]: /posts/terminal-customization/

### Mac OS X

Mac OS X used to be my general development OS because I really liked OS X's font rendering, monospace fonts, and UI aesthetic, but that position has been ceded to Linux for its flexibility and for the various applications becoming available on Linux, mainly Netflix, Air Video Server, myihome, Steam, and Spotify. Now OS X mainly stays around in case I'd like to do some iOS development and to make sure my programs and websites work fine on OS X.

## Editor

I used to believe that people who boasted about using [vim] were just posturing, claiming to work with a seemingly archaic terminal-based editor. However, a friend who first showed me his use of vim through an ssh connection many years ago---when I was barely starting out with software development---managed to show me the level of fluent mastery of text editing that could be achieved with the editor. A few years ago I dedicated time to really learning its vocabulary and customization. The result is that I feel to be in complete control over the text that I'm manipulating. That said, I don't engage in petty text editor wars nor do I attempt to push it on others. I simply use vim to my benefit.

[vim]: http://en.wikipedia.org/wiki/Vim_(text_editor)

### Looks

The truth is that the default configuration of vim is pretty ugly, and so are most of the themes it comes with. This shouldn't really matter, as its benefits greatly outweigh anything cosmetic, but it does matter to me. I use a customized version of [solarized] light; I really dislike the dark version. I primarily use gui versions of vim such as gvim or [MacVim] for their added benefits (e.g. wider range of bindable keys, more colors, etc.). My configuration files [hide the menus and toolbars], making for pretty compact windows.

[solarized]: http://ethanschoonover.com/solarized
[MacVim]: https://code.google.com/p/macvim/
[hide the menus and toolbars]: https://github.com/blaenk/dots/blob/master/vim/gvimrc.ln#L2-L4

### Plugins

I use [many plugins]. I believe the most noteworthy are [vundle] for plugin management, [ctrlp] for fuzzy file opening/switching, [surround] to easily manipulate text's surrounding delimiters, [vim-pandoc-syntax] for Pandoc-flavored markdown highlighting and manipulation, and [YouCompleteMe] for clang-powered auto-complete.

[many plugins]: https://github.com/blaenk/dots/blob/master/vim/vim.ln/conf/bundles.vim
[vundle]: https://github.com/gmarik/vundle
[ctrlp]: http://kien.github.io/ctrlp.vim/
[surround]: https://github.com/tpope/vim-surround
[vim-pandoc-syntax]: https://github.com/vim-pandoc/vim-pandoc-syntax
[YouCompleteMe]: http://valloric.github.io/YouCompleteMe/

## Development Tools

My favorite source control system so far is [git]. I regularly spend time learning about its internals. I use [ag] for searching my files, as an alternative to grep. My compiler of choice is [clang] paired with [GDB]. My [dot files] are up on Github.

[ag]: https://github.com/ggreer/the_silver_searcher
[git]: http://git-scm.com/
[clang]: http://clang.llvm.org/
[GDB]: http://www.gnu.org/software/gdb/
[dot files]: https://github.com/blaenk/dots

## Miscellaneous

[Kindle] for reading books. At first I only used it to read novels and other casual material, but eventually I began using it for more and more technical books, starting with C++ Primer 5th Ed. (and the GDB book and TLPI). I actually used to explicitly prefer physical books, now I consistently find that I can read a lot quicker and have more fun doing it on a kindle. I use [Calibre] to manage my vast ebook library.

[Kindle]: http://amzn.com/B00AWH595M
[Calibre]: http://calibre-ebook.com/

[Chrome] is my primary browser (Chromium on Linux).

[Chrome]: https://www.google.com/intl/en/chrome/browser/

I use [Syncplay] regularly to watch movies with friends, in sync. I use [MPC-HC] on Windows and [mpv] on Linux, both of which work with Syncplay. My friends and I use [mumble] during Syncplay sessions and for any other time we want to communicate.

[mumble]: http://mumble.info/
[Syncplay]: http://syncplay.pl/
[MPC-HC]: http://mpc-hc.org/
[mpv]: http://www.mpv.io/

## Hardware

Most of my life I've had pretty average hardware but I decided to indulge myself with my current system. This is also my first desktop with an Intel Processor.

### Core

* Intel Core i7 2600k @ 4.2 GHz with Cooler Master Hyper 212 Plus
* Asus P8Z68-V Pro
* G.Skill RipJaws Series 4 x 4 GB DDR3 1600
* EVGA GeForce GTX 460 SuperClocked EE 1 GB
* 3 SSDs (120 GB Windows, 60 GB Mac OS X, 60 GB Arch Linux)
* 3 HDDs (2 x 1 TB, 1.5 TB)

### Peripherals

* Dell UltraSharp 23" 1080p IPS
* [Das Keyboard Model S Ultimate] (Mechanical)
* Logitech G500 Laser Mouse
* Cooler Master CM 690 II Advanced
* [Zalman Zm-Mic1 Microphone]
* [Shure SE215-K]

[Das Keyboard Model S Ultimate]: http://www.daskeyboard.com/model-s-ultimate/
[Zalman Zm-Mic1 Microphone]: http://amzn.com/B00029MTMQ
[Shure SE215-K]: http://amzn.com/B004PNZFZ8

*[DE]: Desktop Environment
*[WM]: Window Manager
*[AST]: Abstract Syntax Tree
