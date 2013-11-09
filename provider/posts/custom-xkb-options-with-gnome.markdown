---
title: Custom XKB Options with Gnome
published: November 8, 2013
excerpt: Getting Gnome to obey XKB options
tags: Linux
---

As I've [explained before](/posts/xmonad-ignores-bindings/#media-keys), I use a keyboard without media keys, so I rebind three otherwise-unused keys to act as my media keys. I accomplish this on Gnome by using a custom XKB option that I [created](/posts/xmonad-ignores-bindings/#workaround). I used to enable this XKB option by simply running a script at startup that enabled it, but I noticed that it was subject to either a race condition or a precedence issue. I'm more convinced it was the latter, as if the option that was enabled was overridden by Gnome's own initialization of XKB.

As a result, I decided to look for a way to have my solution work alongside Gnome's initialization of XKB. After searching around, I found that there is indeed a dconf option hidden away at **/org/gnome/desktop/input-sources/xkb-options**. Before I could do this though, I had to make my XKB option available system-wide.

First the option file has to be placed in the symbols directory **/usr/share/X11/xkb/symbols**:

```
partial hidden modifier_keys
xkb_symbols "bottom_right" {
  // 'unbind' the following
  // mute
  key <MUTE> { [ VoidSymbol ] };
  key <I0D> { [ VoidSymbol ] };
  // lower volume
  key <VOL-> { [ VoidSymbol ] };
  key <I0E> { [ VoidSymbol ] };
  // raise volume
  key <VOL+> { [ VoidSymbol ] };
  key <I0F> { [ VoidSymbol ] };

  // now bind the keys I want to use
  key <RCTL> { [ XF86AudioRaiseVolume ] };
  key <MENU> { [ XF86AudioLowerVolume ] };
  key <RWIN> { [ XF86AudioMute ] };
};
```

Now the option has to be listed in the system-wide options list for the layout you're using, in my case evdev, at **/usr/share/X11/xkb/rules/evdev**. Here, volume\_keys corresponds to the filename of the option file, and bottom\_right is the name I gave the group:

```
volume_keys:bottom_right = +volume_keys(bottom_right)
```

Now the option can be referenced system-wide, so it can be entered into the the Gnome XKB options dconf key at **/org/gnome/desktop/input-sources/xkb-options**. Put it inside the list as 'volume\_keys:bottom_right'. In my case because I like to set the right ALT key to act as my compose key, it's set to:

```
['volume_keys:bottom_right', 'compose:ralt']
```

You can now go ahead and bind the appropriate keys to volume functions inside Gnome Setting's keyboard section.

Given that it's tucked away deep inside dconf, and given Gnome developers' track record of pruning "cruft" or anything that the lowest common denominator user doesn't use, the future availability of this option is in question. That said, it currently works perfectly fine.

