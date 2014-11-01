---
title: Terminal Customization
published: February 12, 2013
excerpt: Various urxvt and zsh customizations
tags: Linux
---

* toc

A while back I switched over to [zsh](http://en.wikipedia.org/wiki/Z_shell) as my shell and used [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) to get up and running quickly. I barely used any of the features it provided, so I recently decided to do away with it and get zsh setup from scratch. At the same time I decided it'd be a good idea to do the same for [urxvt](http://en.wikipedia.org/wiki/Rxvt-unicode). These initiatives had the consequence that I ended up completely redoing the way I maintained my dotfiles which had the effect of greatly improving my overall setup.

Before I go any further I'd like to point out that all of the things that I'll talk about in this post are available in my [dotfiles](https://github.com/blaenk/dots) repository. I'll make an effort to link to the relevant individual files from the repository for each topic I cover.

Here is the end result:

<img src="/images/posts/terminal-customization/urxvt.png" class="center">

## dotfiles

I previously had a simple Rakefile that symlinked all of the files in the dotfiles directory into my home directory, except for some in an exception list. However, this had the consequence that I had to have ruby installed beforehand, and I didn't like to install ruby through means other than something like [rbenv](https://github.com/sstephenson/rbenv/). I preferred instead to be able to get my dotfiles up and running as soon as possible on a new system. As a result I opted to use a shell script to deploy my dotfiles.

After looking around in dotfile repositories I found [hoffman's bootstrap script](https://github.com/holman/dotfiles/blob/master/script/bootstrap). The simple script uses `find` to find files and directories whose names end in ".ln" and symlinks them into the home directory. I modified it a bit to use the ".ln" suffix instead of ".symlink"---purely cosmetic of course---as well as some other slight changes. I think I'll change it later so that it can gracefully handle operating system-dependent dotfiles.

## urxvt

My first goal was to get urxvt configured properly. I really didn't like the way stock urxvt looked and operated (e.g. clipboard use), so I set out to learn its configuration format. I ended up defining my own color scheme as well as improving its clipboard support.

### Colors

I initially attempted to replicate the color scheme I used in the OS X terminal, however I found that the very same colors didn't look quite the same in the terminals I tried on Linux (GNOME's or urxvt). I have an IPS monitor which I think has made me pretty sensitive to color. As a result I decided to tweak it a little, and I feel that I've come up with an even better color scheme than before by taking some of the colors from my [vim theme](https://github.com/blaenk/dots/blob/master/vim/vim.ln/colors/blaenk.vim).

I've come to recognize recently that it's pretty easy to go overboard with the amount of colors used in anything, and that oftentimes things tend to look better with a more restricted color palette. My terminal uses more color than your common terminal or prompt, but I think it's all in good taste and for semantic purposes. In fact, I've come to really like the [color scheme I chose](https://github.com/blaenk/dots/blob/master/X11/Xresources.ln#L40), it somehow reminds me of SNES game color palettes.

### Clipboard

Stock urxvt uses the [X Window Selection](http://en.wikipedia.org/wiki/X_Window_selection) copy and paste mechanism. Paste can be done by middle clicking, and copying is on-select. I found a [set of scripts](https://github.com/muennich/urxvt-perls) that take the copy and paste system from barebones to awesome.

The `clipboard` script allows me to copy and paste using the Alt-keys, as in OS X where one can use the Cmd-keys to copy and paste. These are some nice binds to use without interfering with the terminal by sending signals to the current program.

The `keyboard-select` script allows me to go into "visual mode" on the terminal and use vi-bindings to do my copying. This lets me copy text from the terminal without having to leave the keyboard. This paired with my zsh vi-bindings means I never have to leave the home row.

## zsh

This was the bulk of the work, but I quickly realized that it wasn't all that difficult. I didn't want to have one huge zshrc file. Instead I wanted to have specific files for different parts of the configuration, for example `prompt.zsh` would contain configuration for the prompt. I had seen such systems in oh-my-zsh as well as certain peoples' dotfile repositories. I took inspiration from [sunaku's dotfiles](https://github.com/sunaku/home) in which he has a zsh file that sources all of the zsh files in a directory.

### Prompt

My zsh prompt is actually pretty simple. I played around with multiline prompts but I really disliked the feel of them. The zsh file dedicated to [defining the prompt](https://github.com/blaenk/dots/blob/master/zsh/zsh/prompt.zsh) is very clean in my opinion, which is something I strived for throughout its development.

#### Basic

The basic prompt consists of a lambda followed by the path---which [auto-collapses](http://stevelosh.com/blog/2010/02/my-extravagant-zsh-prompt/#current-directory) `$HOME` to `~`---with [highlighted path separators](http://superuser.com/questions/49092/how-to-format-the-path-in-a-zsh-prompt) followed by a right arrow. Pretty simple and easy on the colors in my opinion, aside from the unconventional highlighting of the path separators, which was something I had long wanted and took me a while to get right.

My prompt is inspired by the Haskell [lambda syntax](http://www.haskell.org/haskellwiki/Lambda_abstraction) so that it looks like every line is a lambda operating on the current directory whose code is the command you type:

~~~ {lang="haskell"}
\dir -> command_in(dir)
~~~

Here's what the actual prompt looks like:

<img src="/images/posts/terminal-customization/basic-prompt.png" class="center">

Here it is in text:

~~~ {lang="text"}
λ ~/code/haskell ➜
~~~

**Update**: I have changed my prompt to be multiline. All that has changed is that the ➜ is on its own line.

#### git

Like any self-respecting modern prompt, mine [incorporates git information](https://github.com/blaenk/dots/blob/master/zsh/zsh/vcsinfo.zsh) when within a git repository. It shows the current branch, whether there are any untracked files (denoted by `.`), modified files (denoted by `#`), or staged files (denoted by `+`). Another nice little thing I added was a marker for how far ahead and/or behind we are from the remote branch. This is appended to the aforementioned information and only shows up when we _are_ ahead or behind.

All of these features were done using zsh's [vcsinfo](http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#Version-Control-Information) with the help of [Arjan van der Gaag](http://arjanvandergaag.nl/blog/customize-zsh-prompt-with-vcs-info.html) and, once again, [sunaku](https://github.com/sunaku/home/blob/master/.zsh/config/prompt.zsh). Here's what it looks like [^echo]\:

<img src="/images/posts/terminal-customization/git-prompt.png" class="center">

Again in text:

~~~ {lang="text"}
λ ~/.dots (master . # +){+1} ➜ 
~~~

The branch is `master` and apparently there's unstaged files, modifed files, and staged files. Furthermore, the `{+1}` shows that my branch is one commit ahead of the remote branch. It would also show the number of commits we are behind, in red, if that were the case---which in my opinion can be very handy so that you can avoid conflicts by rebasing or merging before you push. This segment only shows up if either of those conditions is met.

#### SSH

The prompt also detects if it's being viewed through SSH. I don't like viewing hostname in my prompt on machines I'm on locally as I feel it's pointless. However, when I'm connected to a remote server via SSH, it's often handy to have the hostname around to differentiate between your computer and the remote host. For this reason my prompt only shows the hostname when it detects that it's being viewed through an SSH connection. Here's what it looks like:

<img src="/images/posts/terminal-customization/ssh-prompt.png" class="center">

Once again in text:

~~~ {lang="text"}
[someserver] λ ~/.dots (master) ➜ 
~~~

**Update: October 28, 2013**

I've actually done away with this component of the prompt. I didn't like how the lambda no longer aligned with the arrow symbol. I've instead decided to add a pretty simple green `R` at the end of the current working path, to signify that I am on a "remote" machine, so it reads something like "currently on x path remotely."

```
λ ~/.dots R (master)
➜
```

### vi-Binds

One thing that I can't live without now when using vi-bindings is binding `jj` to vi-mode. The default key for this is Escape, but Escape is [used for other hotkeys](http://unix.stackexchange.com/questions/23138/esc-key-causes-a-small-delay-in-terminal-due-to-its-alt-behavior) that the terminal (or shell?) intercepts. For this reason, a single keypress of Escape introduces a bit of lag, which I imagine is required to differentiate a hotkey (i.e. `Esc-C`) from a simple Escape keypress.

Binding to `jj` has the consequence of being more accessible. In fact, this is a common bind that people tend to use in vim for this very reason. Before coming to this realization I mainly used Emacs-binds because they didn't introduce lag. However, with this new bind I'm able to jump into vi-mode and edit commands very quickly and more intuitively (for a vim user like myself).

### Highlighting

If you've been wondering how it is that my commands are highlighted, it's made possible by [this highlighting script](https://github.com/zsh-users/zsh-syntax-highlighting). Simply sourcing that script into your zsh environment suddenly colors your commands. In my opinion it makes the terminal look much better without going overboard with the colors. I did have to [tweak some settings](https://github.com/blaenk/dots/blob/master/zsh/zsh/highlight.zsh) though because I felt that the default configuration did go a bit overboard on the styling, such as underlining program names.

## Conclusion

I have to say that I love the way my terminal looks and operates now. This has all been in preparation for setting up [xmonad](http://en.wikipedia.org/wiki/xmonad), which I intend to take a shot at soon. For the curious, I'm doing all of this on [arch linux](http://www.archlinux.org/), but I didn't say it earlier because nothing in this post depends on this. In fact, none of the content in this post is all that specific to Linux itself even. I'll try to update this post whenever my setup changes, but I think I'm quite comfortable with the way it is, and will be for the foreseeable future.

In my opinion, the benefit of using such tried and proven, mature tools is that once you take the time to configure them to your liking you can use them without modification for a long time. Barring some imminent, majorly disruptive paradigm shift in software development, I can see myself using many of these tools throughout my career as a software developer.

[^echo]: Ignore the `echo` command. I used it to give the current line some padding to make taking a screenshot a little bit easier.
