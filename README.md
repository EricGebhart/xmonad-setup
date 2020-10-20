# xmonad-setup
my setup files for xmonad

I've been using xmonad since July, 2014

This is a complete configuration, it does not really use the default setup.  All keys are defined here. I did
stick to the defaults to a point.  But this setup uses sub-menus for the keys to provide more control.
It also provides a popup menu of keystroke possibities, very much like emacs. So it's easy to find what keystrokes do what.

Xmonad is a big universe. The dzen key hints really helps with exploring and remembering what is there.  
Make a submap of commands you wonder about, try them out. Rearrange as needed.

`M4-S-m` to get a pop up of the top level key commands.

I use arch linux, so these are also the arch package names. 
See my (arch-pkgs repo)[http://github.com/ericgebhart/arch-pkgs] for a meta package that will install most of this.
There, just do a `make Xmonad` and you should be good for packages from the AUR or otherwise. 

There are lots of choices for status bars around. I've used xmobar, xfce-panel, kde-panel, and now I'm back
to what seems the simplest and best. polybar.   For all the various desktop panels see my xmonad-log-applet repo.

I recently added rofi, but I'm not sure it's any better than dmenu. I know it's not _just_ dmenu, but that's all
that I can think of to use it for. Conky is fine and pretty, but htop is just so simple.
Both work. Conky is put into a scratchpad so it can be easily managed and invoked.

This repo is for configurations and etc. Just do a `make all` here.  If you need to install things 
and you use an Arch Linux based distro then go get my [arch-pkgs](http://github.com/ericgebhart/arch-pkgs) `make Xmonad` there will install everything you need.
See my other more comprehensive setup repo: [Arch-Setup](http://github.com/ericgebhart/Arch-Setup) 

You will need:
* xmonad
* xmonad-log-applet (from my repo)[http://github.com/ericgebhart/xmonad-log-applet]
* dzen2
* dmenu
* rofi
* polybar
* conky
* rxvt-unicode - or change xmonad.hs to point at your favorite terminal.
* termite - or change xmonad.hs to point at your favorite terminal.
* xcompmgr or compton if you want transparency to work.
* adobe-source-code-pro-fonts
* ttf-ubuntu-font-family
* awesome-terminal-fonts - perhaps.
* ghci is nice to have for haskell coding. ie. if you modify xmonad.hs.
* feh        -- set background image.
* For touch screens
    * onboard    -- on screen keyboard 
    * easysroke  -- gesture support.

A couple of things about keys, I'm still figuring what I want, but one of the things that effects my decisions is that I use a dvorak keyboard.  I put my favorite commands on or close to the home row, which may not make sense on a qwerty keyboard. One menu that really stands out as dvorak specific is the floating window sub menu. It has commands on the right hand to place windows in 9 different places as if your right hand was on a keypad. Another difference
is any time someone might be tempted to use `hjkl` as arrow keys I use `htns` which is the right hand home row.

If there is a grid-select, there is probably also a submap key menu and a prompt version as well.
Grid Select uses the default navigation, vi, arrows, and / to search. Enter or click to choose, Escape...

**Some key features**  
 * EZConfig - Emacs style keybindings and submenus.
 * Key hints - Emacs style popup of key command choices.
 * topic space -- automatically start applications when going to a workspace
 * per-workspace layouts -- different default layouts and layout choices by workspace
 * grid select  - popup menu of choices.
   * workspaces
   * layouts
   * windows
   * shift window to workspace
   * bring window to workspace
   * named scratchpads
   * Prompted & Selected search, man, duckduckgo, hoogle, Arch, Wiktionary, etc.
   * Applications - never use.
 * Window marking/tagging.
 * prompt & select search 
 * other prompts - man prompt, shell prompt,
 * floatkeys - resize and move floating windows.
 * magnifier layouts 
 * named scratchpads , terminals, bc, Htop, conky, pavucontrol, alsamixer, ghci
 * working screenshot
 * dmenu / rofi
 * polybar, xfce-panel
 * cellwriter  - if you want pen gesture support.
 
 
 Any custom scripts I use are in my *necessities* package.  emacsn and bcl my bc calculator setup are there among others.
*emacsn*
 is script to start emacs as client, or standalone, and to call an elisp function on startup. -- I use standalone emacs running mu4e for email and a separate emacs session for coding. Sometimes I want coding sessions to be homogoneous through emacs client and sometimes I want them standalone. This script takes care of that for me in a nice way.

*xfce-panel*
I no longer use this. I use polybar instead. If you wish to use xfce-panel with dbus etc. then
see my [xmonad-log-applet](https://github.com/EricGebhart/xmonad-log-applet) repo to get xfce-panel  to display xmonad information.

Key Hints
---------
Emacs style popup key hints via dzen2, 
thank you [pclewis!](https://github.com/pclewis/dotfiles/tree/master/xmonad/.xmonad) for the original idea and code!

The important bits for keyhints is `showHintForKeymap.sh`, an awk program that lives in your .xmonad directory. 
If things are displayed too tightly, or dropping off, it may be necessary to increase the line height
in your xmonad.hs.  Hopefully it's obvious.
 
If you want to change the font for dzen something like this works in .Xresources. You can also pass the font in to the showhintforkeymap.sh function.  -- all of this is taken care of with the install. But mind that I have a hi-dpi monitor and I have settings for that as well. So *big* fonts sometimes. You'll want to read the code...

Everything you need is in this project, so go explore.

```dzen2.font: -*-ubuntu mono-*-*-*-*-*-*-*-*-*-*-*-*```

 All the fonts and colors are near the top of xmonad.hs with the rest of them. So adjust accordingly.

Creating a submap key menu is easy. The key hint label will be the name of the keymap variable, or the comment on the first line.
In the example below "Raise" will be the title. Without the comment "raiseKeymap" would be the title. 
If there is a comment at the end of a keymap entry that 
will be used as the descriptor in the keyhint.  Otherwise the actual command will be used.
Formatting should be nice, but the awk parser is fairly tolerant. It works by looking through your
xmonad.hs for the keymap name and grabbing everything from there to the closing **]**

```
mainKeymap c = mkKeymap c $ -- Main Keys
    [
    ...

    ("M4-r",          toSubmap c "raiseKeymap" raiseKeymap)

    ...
    ]

raiseKeymap = -- Raise
    [ ("v", runOrRaiseNext "Vivaldi" (className =? "Vivaldi")) -- Vivaldi
    , ("e", raiseNext (className =? "Emacs")) -- Emacs cycle
    , ("s", runOrRaise "Slack" (className =? "Slack")) -- Slack
    ]
```

This will result in a menu something like this when the M4-r keys are pressed.
```
                          Raise
    v Vivaldi       e Emacs cycle        s Slack
```

BC Scratchpad
-------------

For more information see my repository (bc-extensions)[https://github.com/ericgebhart/bc-extensions]
This is also part of my [Arch-Setup repo](https://github.com/ericgebhart/Arch-Setup)which has makefiles and repos for all sorts of things.


Install
-------
Just do a `make all`  But if you want to be cautious and know what is going where, read the Makefile.
Maybe you have a .xresources, a .xinitrc, your own xmonad.hs and other stuff.
Everything is here, so have at it.

To install just the xmonad parts use 'make install'. To install the xsession use 'sudo make xsession'.  
This does reference the xmonad.start in your home directory, so perhaps you'll want to
put that in a more generic place.  I usually just do `startx ./xmonad.start` in my _.zlogin_.
Optionally, use the .xinitrc which is exactly the same. But allows for a display manager or just `startx`.
Whatever you want...

That should be enough rattling around, have fun.

