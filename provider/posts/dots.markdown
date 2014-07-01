---
title: Dots
published: February 16, 2014
excerpt: How I manage my dotfiles
tags: Linux
---

* toc

It's become a common practice to keep one's [dotfiles] version controlled, oftentimes mirrored somewhere like Github. A lot of people start their own dotfile collection based (forked) off of someone else', but that never felt right to me. In my view, dotfiles are personal, sensitive configuration files that are explicitly defined to one's own taste. It never made sense to me to want to use a giant wad of files with who knows what configuration directives in there. It seemed a lot like [cargo culting] to me.

[cargo culting]: http://en.wikipedia.org/wiki/Cargo_cult_programming

I do enjoy looking at other peoples' dotfiles from time to time to see what I can pick out and adapt to my own, but otherwise I like to grow mine organically---that is, only configure what I need, as I need it---to ensure that I really understand my configuration. As a result, I created [my own dotfiles] from scratch.

[dotfiles]: http://en.wikipedia.org/wiki/Dotfile#Unix_and_Unix-like_environments
[my own dotfiles]: https://github.com/blaenk/dots

## File Structure

The file structure is pretty simple. There's a folder for every type of dotfile collection, for example: zsh, git, vim, and so on. Each of these can contain hidden files and folders that can be deployed by the deploy script.

## Deployment

My deploy script is called `sprinkle`, which is a heavily modified fork of the deploy script from [holman's dotfiles]. I chose this script to start from because I liked that it was a shell script, unlike most other deploy scripts I had seen which were Rakefiles, naturally requiring Ruby to be installed on the system before being able to deploy the dotfiles.

[holman's dotfiles]: https://github.com/holman/dotfiles

I also liked that the deploy script had a naming convention such that files and folders with a `.symlink` suffix were those that were deployed. However, I didn't like that vim and Github wouldn't detect the filetype---and therefore wouldn't highlight---due to the misleading file extension. So I ended up heavily customizing the script.

Now, instead of the `.symlink`-suffix naming convention, those files and folders that should be deployed are themselves hidden. This allows vim, Github, and others to detect the file type and provide highlighting. Running the deploy script for the first time yields something like this:

<img class="center" src="/images/posts/dots/deploy.png">

My zsh files have an alias for the sprinkle script so that it can be run from anywhere, though in this case I was already in my dots directory. Files that haven't been deployed are immediately deployed (symlinked) unless there's an existing file in the destination. In that case, there are options to backup, overwrite, or remove (without deploying) the existing file, as well as skip that file altogether.

These commands are entered when prompted by simply entering the first letter of the action, i.e. `o` for overwrite. A capitalized letter performs that action for all remaining files as well. This is what the prompt looks like:

<img class="center" src="/images/posts/dots/prompt.png">

## tmux

My tmux configuration is pretty simple I think. I keep the bind at `C-b` though it kind of interferes with `C-f/C-b` scrolling, in which case there's a bit of lag for `C-b` unless I tap it twice. Instead I'm getting used to scrolling with `C-u/C-d` though it's pretty disorienting starting out.

My window list is pretty subtle I think. Active windows are underlined and the current window name is emboldened.

<img class="center" src="/images/posts/dots/tmux.png">

I have binds for creating new windows with `M-n` and renaming a window with `M-r`:

```
bind -n M-r command-prompt 'rename-window %%'
bind -n M-n command-prompt -p "Name of new window:" "new-window -n '%%'"
```

I also created simple binds for navigating and moving windows around. `M-h/M-l` moves to the left and right window respectively, and `M-j/M-k` moves the current window left and right, respectively.

```
# switch between windows left/right
bind -n M-h previous-window
bind -n M-l next-window

# move windows left/right
bind -n M-j swap-window -t -1
bind -n M-k swap-window -t +1
```

## zsh

My zsh configuration files are created from scratch as well, I don't use something like oh-my-zsh for the same reasons that I stated in the opening paragraph. I do use [antigen], which is similar to [vundle] but for zsh, mainly to avoid having to either keep stale snapshots or juggle git submodules of zsh plugins I use. I only use two zsh plugins: [syntax-highlighting] and extra/community [completions].

[antigen]: https://github.com/zsh-users/antigen
[vundle]: https://github.com/gmarik/Vundle.vim
[syntax-highlighting]: https://github.com/zsh-users/zsh-syntax-highlighting
[completions]: https://github.com/zsh-users/zsh-completions

I then have a separate zsh sub-folder that stores zsh files that configure different aspects of zsh, such as aliases, completions, functions, bindings, and so on.

### Aliases

One of my most used aliases is `:q` which is simply aliased to `exit`, making it very natural for me to exit shells. If `pacman` is present on the system, I create many aliases to different kinds of `pacman` commands, such as `pacup` for `pacman -Syu`, `pacin` for `pacman -S`, and so on.

### Completions

There's nothing really special about my completions configuration. I do set it up to use my `dircolors` setup, so that the completion menu uses the correct colors for the different kinds of files. I also set it up for case-insensitive substring completion, so that I can type a bit of text from anywhere in the filename, regardless of case, and have it tab-complete correctly.

### Functions

I do have a few functions I find to be very useful. The first is one that opens up a man-page directly to a given flag, i.e. `manf ls -l` opens the man-page for `less` directly to the point that describes the `-l` switch.

``` bash
function manf() {
  man -P "less -p \"^ +$2\"" $1
}
```

Another one of note is one I found on stackoverflow which basically prepends a column to `ls -lh` which contains the permissions of the files in octal/numerical form. I let this take the place of the regular `ls` command, which can still be run for whatever reason by prepending it with `command`:

``` bash
function ls() {
  command ls -lh --color=always $@ |\
    awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/)\
         *2^(8-i));if(k)printf("%0o ",k);print}'
}
```

Example output:

```
total 32K
755 drwxr-xr-x 2 jorge users 4.0K Feb 15 20:05 git
644 -rw-r--r-- 1 jorge users    0 Jan 27  2013 README.md
755 drwxr-xr-x 2 jorge users 4.0K Feb 13 00:54 ruby
```

I also created a wrapper around the built-in `cd` function that accepts parameters of the form `b...` where the number of dots is arbitrary. If the wrapper detects a parameter of that form, it removes the `b` prefix and expands each of the dots to `../`. This makes for very quick navigation up an arbitrary amount of directories. The actual change-directory work is delegated to the original built-in `cd` function:

``` bash
function cd() {
  emulate -LR zsh

  if [[ $1 == 'b.'* ]]; then
    builtin cd ${${1/"b"}//"."/"../"}
  else
    builtin cd $*
  fi
}
```

Example usage:

``` bash
some/dir/here $ cd b..
some/ $ # went up two directories
```

I also have a function for listing the pacman orphan packages on my system, i.e. those that aren't required by any other package. I found a command for doing this but it just dumped a list of every package, so I modified it to also list the description of the package. This requires the `expac` package, a utility to query the pacman database:

``` bash
function pacorphans() {
  expac "%n:%N:%d" -Q $(expac "%n %G" | grep -v ' base') |\
    awk -F: '$2 == "" {printf "%s: %s\n", $1, $3}'
}
```

Example output:

```
yasm: A rewrite of NASM to allow for multiple syntax supported (NASM, TASM, GAS, etc.)
zsh: A very advanced and programmable command interpreter (shell) for UNIX
```

Finally, another function I recently created that I find very useful is one to fetch my external IP address and both copy it to my clipboard and print it out to the terminal. This is very useful because my IP address does change from time to time, usually if I restart my router. Considering that I host a [mumble] and [syncplay] server, whenever this happens I have to inform my friends of the change, which usually requires me to manually determine my external IP by going to some website that provides the information.

[mumble]: http://mumble.sourceforge.net/
[syncplay]: http://syncplay.pl/ 

So I decided to create a command that gets the IP address from [ipinfo.io], copies the response to my clipboard, and also prints it out to the terminal.

[ipinfo.io]: http://ipinfo.io

In the past, one would use a command such as `ifconfig` to list the computer's local addresses. Recently there has been a shift to use the new [`ip`][ip] command which houses many sub-commands such as `ip addr` which is now the preferred method to list addresses.

[ip]: http://man7.org/linux/man-pages/man8/ip.8.html

So what I did was create a wrapper for this `ip` command and create a fake sub-command called `get` which performs this task of retrieving my external IP address. If the sub-command `get` wasn't provided, then the wrapper delegates the work to the actual `ip` command, if it exists.

``` bash
function ip() {
  emulate -LR zsh

  if [[ $1 == 'get' ]]; then
    res=$(curl -s ipinfo.io/ip)
    echo -n $res | xsel --clipboard
    echo "copied $res to clipboard"
  else
    # only run ip if it exists
    if (( $+commands[ip] )); then
      command ip $*
    fi
  fi
}
```

Example output:

``` bash
λ ~/some/place
» ip get
copied 123.45.678.90 to clipboard
```

### Cursors

I setup mode-aware cursors in zsh, to better emphasize when I'm in vi mode and not. This is pretty straightforward, simply sending the correct terminal control sequence:

``` bash
function zle-keymap-select {
  zle reset-prompt

  if [[ $KEYMAP = "vicmd" ]]; then
    echo -ne "\033]12;10\007"
  else
    echo -ne "\033]12;6\007"
  fi
}

function zle-line-finish {
  echo -ne "\033]12;6\007"
}

zle -N zle-keymap-select
zle -N zle-line-finish
```

This works perfectly fine in urxvt, but tmux must be configured to allow this because otherwise the setting of the cursor color above by zsh bypasses tmux, applying to tmux as a whole. This means that if one tmux window is in vi mode, the cursor will change, but if one then switches to another tmux window that is in insert mode, the cursor color for that window will remain the same as in the one in vi-mode. That is, the changed cursor color applies to every screen in tmux.

tmux did implement functionality for it to remember the cursor color on a per-window basis back in 2011, but this is only configured out of the box for xterm, since every terminal's control sequences may vary.

The cursor color is inherently global, so what happens is that tmux remembers the cursor color for every window. When switching to another tmux window, tmux checks if that window's cursor color had been previously changed. If so, tmux sets the global cursor color to that window's saved cursor color. Otherwise, it means that that window's cursor color hasn't been changed, in which case it needs to reset the cursor color to the "default" cursor color, in case the previous window did change the color.

For this, two terminal escape sequences have to be defined, or overridden: the first tells tmux how to set the cursor color and the other tells tmux how to reset it to the "default" color.

The sequence for setting the color is the same in xterm and urxvt: `\033]12;color\007`. However, there is no sequence I know of---after looking at `man 7 urxvt`---for resetting the cursor color to the default cursor color. For xterm, it is `\033]112\007`. So instead what I decided to do was tell tmux that the sequence was simply the one to set the color, but with the default cursor color explicitly defined, which for me is the 6th ANSI color code (cyan).

```
set -g terminal-overrides ',rxvt*:Cs=\E]12;%p1%s\007:Cr=\E]12;6\007'
```

### Misc

When one runs a command that doesn't exist, it generally gives an error pointing out that fact. However, the `pkgfile` package provides a zsh script that, when sourced, provides information about which package such a command may be found in.

``` bash
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] &&\
  source /usr/share/doc/pkgfile/command-not-found.zsh
```

Example output:

``` bash
λ ~/some/place
» clojure
clojure may be found in the following packages:
  community/clojure 1.5.1-2     /usr/bin/clojure
```

I also configured [highlighting] for the `less` pager.

[highlighting]: https://github.com/blaenk/dots/blob/master/zsh/zsh/highlight.zsh

<img class="center" src="/images/posts/dots/less.png">

My prompt is pretty involved and it's discussed more in-depth in my [customization] post, though it's slightly outdated. See my dotfiles for the latest configuration.

[customization]: /posts/terminal-customization/#prompt

One noteworthy thing however is that I highlight the path separator in a subtle cyan.

``` bash
SLASH="%{$fg[cyan]%}/%{$reset_color%}"
echo "${${PWD/#$HOME/~}//\//$SLASH}"
```

My vi-binds are pretty straightforward. One noteworthy thing is that I bound `_` and `g_` to go to the beginning and end of line, respectively, to reflect what I use in vim.

``` bash
bindkey -M vicmd "_" beginning-of-line
bindkey -M vicmd "g_" end-of-line
```

I also created some binds specific to the completion menu. I bound `shift-tab` to go in the reverse direction of `tab`. I also changed `Enter` to accept and enter the command, instead of the default, which only accepts the completion and allows the user to continue typing the command. Instead, I bound `C-g` to perform the accept-only:

``` bash
bindkey -M menuselect "^M" .accept-line
bindkey -M menuselect "^G" accept-line
bindkey -M menuselect "^[[Z" reverse-menu-complete
```

## vim

My vim configuration is discussed in other posts, such as [this one]. Some noteworthy things are my mode-aware cursors which you can see [here], where my statusline is also discussed. They're basically color-coded based on the mode. I also made all of the cursors be the block cursor, rather than I-beam for insert mode as is default. I also disabled cursor blinking:

[this one]: /posts/a-simpler-vim-statusline/
[here]: /posts/a-simpler-vim-statusline/#redesign

``` vim
set gcr=a:block

" mode aware cursors
set gcr+=o:hor50-Cursor
set gcr+=n:Cursor
set gcr+=i-ci-sm:InsertCursor
set gcr+=r-cr:ReplaceCursor-hor20
set gcr+=c:CommandCursor
set gcr+=v-ve:VisualCursor

set gcr+=a:blinkon0

hi InsertCursor  ctermfg=15 guifg=#fdf6e3 ctermbg=37  guibg=#2aa198
hi VisualCursor  ctermfg=15 guifg=#fdf6e3 ctermbg=125 guibg=#d33682
hi ReplaceCursor ctermfg=15 guifg=#fdf6e3 ctermbg=65  guibg=#dc322f
hi CommandCursor ctermfg=15 guifg=#fdf6e3 ctermbg=33  guibg=#268bd2
```

I also enable the cursorline only on the current window:

``` vim
augroup cursorline
  autocmd!
  autocmd VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline
augroup END
```

Instead of letting CtrlP generate the list of files, which can be slow, I delegate this work to `find` and `dir` on unix and windows respectively. If we're within a git repository, then I take advantage of `git ls-files` to do this instead.

``` vim
let s:ctrlp_fallback =
  \ has('win32') ?
    \ 'dir %s /-n /b /s /a-d' :
    \ 'find %s -type f'

let g:ctrlp_user_command = [
  \ '.git',
  \ 'git --git-dir=%s/.git ls-files -co --exclude-standard',
  \ s:ctrlp_fallback
  \ ]
```

## Conclusion

All in all it's a pretty simple dotfiles system I think. Feel free to take a look through it to see what you may like to adapt to your own configuration.
