---
title: Custom XKB Options with Gnome
published: November 8, 2013
excerpt: Getting Gnome to obey XKB options
tags: Linux
---

As I've [explained before](/posts/xmonad-ignores-bindings/#media-keys), I use a keyboard without media keys, so I rebind three otherwise-unused keys to act as my media keys. I accomplish this on Gnome by using a custom XKB option that I [created](/posts/xmonad-ignores-bindings/#workaround). I have to do this because the Gnome keyboard settings binder ignores the keys I want to bind for this particular purpose. I used to enable this XKB option by simply running a script at startup that enabled it, but I noticed that it was subject to either a race condition or a precedence issue. I'm more convinced it was the latter, as if the option that I enabled was overridden by Gnome's own initialization of XKB. This suspicion seems to be confirmed by the Arch wiki page on [keyboard configuration](https://wiki.archlinux.org/index.php/Keyboard_Configuration_in_Xorg):

> This article describes low-level configuration using XKB which is effective in most cases, but some desktop environments like GNOME override it with its own settings.
>
> <cite><strong>Arch Wiki</strong></cite>

As a result, I decided to look for a way to have my solution work alongside Gnome's initialization of XKB. There was _very_ little information on Gnome's interaction with XKB short of reading Gnome's source, but after searching around for a long time, I found that there is indeed a dconf option hidden away at `/org/gnome/desktop/input-sources/xkb-options`{.path}. Before I could use this though, I had to make my XKB option available system-wide.

First the symbol file has to be placed in the symbols directory `/usr/share/X11/xkb/symbols`{.path}. This file simply binds the keys I want to use to the XF86 designated media keys. This way the Gnome binder will pick them up without complaining when I bind them to the volume keys, since it otherwise seems to prevent the binding of these very keys as they are. In other words, with this symbol file, RCTRL will be interpreted by the system as if I had pressed XF86AudioRaiseVolume, which is what an actual volume-up media key would yield:

```
partial hidden modifier_keys
xkb_symbols "bottom_right" {
  key <RCTL> { [ XF86AudioRaiseVolume ] };
  key <MENU> { [ XF86AudioLowerVolume ] };
  key <RWIN> { [ XF86AudioMute ] };
};
```

Now an option has to be created for this symbol file and then listed in the system-wide options list for the rule set you're using, in my case evdev, at `/usr/share/X11/xkb/rules/evdev`{.path}. Here, `volume_keys` corresponds to the filename of the symbol file, and `bottom_right` is the name I gave the group:

```
volume_keys:bottom_right = +volume_keys(bottom_right)
```

Now the option can be referenced system-wide, so it can be entered into the the Gnome XKB options dconf key at `/org/gnome/desktop/input-sources/xkb-options`{.path}. Put it inside the list as `'volume_keys:bottom_right'`, for example mine is set to:

```
['volume_keys:bottom_right', 'compose:ralt']
```

You can now go ahead and bind the appropriate keys to volume functions inside Gnome Setting's keyboard section.

Given that it's tucked away deep inside dconf, and given Gnome developers' track record of pruning "cruft" or anything that the lowest common denominator user doesn't use, the future availability of this option is in question. That said, it currently works perfectly fine.

**Update**: On archlinux, `pacman -Qo` shows that the `evdev`{.path} file is owned by the [xkeyboard-config] package. Whenever this package is updated, it overwrites this file, necessitating the change to be added once again. I'll have to look into a more resilient way to have this setup.

[xkeyboard-config]: https://www.archlinux.org/packages/extra/any/xkeyboard-config/
