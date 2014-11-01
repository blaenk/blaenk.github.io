---
title: xmonad Ignores Bindings
published: February 24, 2013
excerpt: Tracking down a bug in xmonad
tags: Linux
toc: off
---

In my [previous post](/posts/terminal-customization/) I talked about how I spent a while configuring my system, specifically urxvt and zsh, in preparation for setting up [xmonad](http://xmonad.org). I've finally gotten around to setting up xmonad. One problem in particular stopped me from continuing with the rest of the configuration.

**Update**: Shortly after posting in the [issue tracker entry](https://code.google.com/p/xmonad/issues/detail?id=273) for this issue relating my experience and affirming that the proposed patch fixed the problem, the gracious developers merged the patch into the main tree. This problem should no longer affect anyone!

## Media Keys

I have a regular keyboard layout, [Das Keyboard Model S Ultimate](http://www.daskeyboard.com/model-s-ultimate/), which lacks media keys (i.e. volume up, down, etc). This wasn't too much of a problem when I used headsets because most of them have dedicated volume controls. However, I got tired of headsets being rendered useless when any little thing messed up (e.g. microphone, a speaker, etc).

As a result I ended up buying a [cheap standalone mic](http://amzn.com/B00029MTMQ) and now use my iPhone's [Shure SE215-K](http://amzn.com/B004PNZFZ8) earbuds for sound on my computer. This is very easy to do given my computer case' front panel audio connector. Of course, the problem now is that there aren't any dedicated media keys and having to use a GUI to change the volume is cumbersome.

My solution to this problem in Windows and Mac is to bind the bottom right keys to media keys as follows:

Key            Purpose
---            -------
Right Control   Volume Up
Menu            Volume Down
Right Windows   Volume Mute

## Binding

Creating these binds is possible on Windows via a registry hack, facilitated using a program such as [SharpKeys](http://www.randyrants.com/sharpkeys/).

On Linux I initially did this using `xmodmap`:

~~~ {lang="text"}
remove Control = Control_R
keycode 105 = XF86AudioRaiseVolume
add Control = Control_R

keycode 135 = XF86AudioLowerVolume

remove mod4 = Super_R
keycode 134 = XF86AudioMute
add mod4 = Super_R
~~~

Binding to these `XF86Audio*` keys automatically adds support for these keys in different applications like [mplayer2](http://www.mplayer2.org/), but I wanted system-wide volume support. This is typically accomplished by wiring them up in your given Desktop Environment or Window Manager. So I went ahead and did so in `xmonad.hs`:

~~~ {lang="haskell"}
((0, xF86XK_AudioMute), spawn "amixer -q set Master,0 toggle"),
((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master,0 5%- unmute"),
((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master,0 5%+ unmute")
~~~

## The Problem

The problem was that xmonad would only react to the Right Control key (Volume Up). However, `xev` correctly interpreted the keys as having been bound to the `XF86Audio*` keys. I was really confused as to why the binds apparently did work at the system level but only one of them worked at the window manager level.

To rule out that it wasn't something with the system-level (xmodmap) binds, I decided to check if it worked in [Awesome](http://awesome.naquadah.org/):

~~~ {lang="lua"}
awful.key({}, "XF86AudioLowerVolume", function () awful.util.spawn("amixer -q set Master,0 5%- unmute", false) end),
awful.key({}, "XF86AudioRaiseVolume", function () awful.util.spawn("amixer -q set Master,0 5%+ unmute", false) end),
awful.key({}, "XF86AudioMute", function () awful.util.spawn("amixer set Master,0 toggle", false) end),
~~~

Indeed it worked perfectly. So now I had narrowed down the problem to xmonad.

## Bug Hunting

Eventually I decided to stop by `#xmonad` on freenode. There I found Paul Fertser who spent the next ~6 hours helping me track down what he figured to be a bug in xmonad. I told him that the system-level binds did work, but not in xmonad. I showed him my binds using `xmodmap -pke`.

He noticed that the `XF86Audio*` keys were bound twice: once by default by XKB (`xmodmap`'s more modern replacement) bound to the keycodes I would have if my keyboard had media keys, and bound again to the keys I chose (the bottom right keys). He then hypothesized that xmonad wasn't grabbing the keys at all due to Xlib limitations. Specifically, the `XKeysymToKeycode` function only returns one keycode per key, biased towards lower keycodes, presumably due to an increasing iterative search of the keycodes for a match.

This theory accounted for why the Right Control (Volume Up) bind did work and not the others. What happened was that Right Control's keycode was lower than the duplicate bind's keycode. As a result, when xmonad used `XKeysymToKeycode` it retrieved the correct keycode. The other two binds, however, have higher keycodes than the default-bound ones, and so `XKeysymToKeycode` returned the first (lower) keycode it found and as a result xmonad never even knew of the other binds' existence.

To test this theory, Paul had me run [`ltrace`](http://en.wikipedia.org/wiki/Ltrace) on xmonad to see which keys xmonad grabbed. The output of this clearly showed that xmonad only grabbed the keys with the lower keycodes.

## Workaround

Now that we were pretty sure of the cause of this, the workaround was to remove the other keycodes (for keys I didn't even have on my keyboard). At this time I decided I might as well switch over to XKB. The first order of business was to [dump my XKB map](http://unix.stackexchange.com/a/65600/10163):

~~~ {lang="bash"}
$ setxkbmap -print > ~/.xkb/keymap/mymap
~~~

Then I created a `~/.xkb/symbols/volume_keys` file to store my media key binds. It took me a long while to figure out how to remove/unbind the default-bound keys. One problem was that XKB sets different aliases for keys. For example, `<I0D>` (I guess that's a media key) was aliased to `<MUTE>`. I looked around in `/usr/share/X11/xkb/rules/evdev` to see what was aliased and made sure to unbind those too. As for unbinding, at first Paul suggested to bind the keys to `NoSymbol` but that apparently had no effect. Eventually I found out it was possible with [`VoidSymbol`](http://madduck.net/docs/extending-xkb/#attaching_symbols_to_keys).

~~~ {lang="text"}
partial modifier_keys
xkb_symbols "volume_keys" {
  // mute
  replace key <MUTE> { [ VoidSymbol ] };
  replace key <I0D> { [ VoidSymbol ] };

  // lower volume
  replace key <VOL-> { [ VoidSymbol ] };
  replace key <I0E> { [ VoidSymbol ] };

  // raise volume
  replace key <VOL+> { [ VoidSymbol ] };
  replace key <I0F> { [ VoidSymbol ] };

  replace key <RCTL> { [ XF86AudioRaiseVolume ] };
  replace key <MENU> { [ XF86AudioLowerVolume ] };
  replace key <RWIN> { [ XF86AudioMute ] };
  replace key <RALT> { [ Multi_key ] };
};
~~~

Now I loaded my XKB map in `~/.xinitrc`:

~~~ {lang="bash"}
$ xkbcomp -I$HOME/.xkb ~/.xkb/keymap/mymap $DISPLAY
~~~

I restarted xmonad with `Mod-Shift-Q` (so that `~/.xinitrc` is rerun) and everything now worked perfectly.

## Bug Report

Over the course of my transition to XKB, Paul found that there was already [an issue](https://code.google.com/p/xmonad/issues/detail?id=273) opened back in 2009 concerning this. The issue report has a patch attached that fixes this, but the patch has yet to be applied to xmonad. Paul suggested I try the patch myself and communicate my results back to the issue report. So I went ahead and got xmonad and xmonadContrib from the darcs repository, ran a simple `darcs apply keycode.dpatch`, and installed each with a `--prefix` to prevent clashing with the ones already installed with pacman. Indeed, the patch worked perfectly.
