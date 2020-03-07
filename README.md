# xmonad-setup
my setup files for xmonad

I've been using xmonad since July, 2016. 

This is a complete configuration, it does not really use the default setup.  All keys are defined here. I did
stick to the defaults to a point.  But this setup uses sub-menus for the keys to provide more control.
It also provides a popup menu of keystroke possibities, very much like emacs. So it's easy to find what keystrokes do what.


The keys and commands are still in a lot of flux. But I'm slowly exploring everything I have and figuring out what I like and
don't like. The dzen key hints really helps with exploring and remembering what is there. And also finding things
that don't work quite the way they should or the way I want.

I use arch linux, so these are also the arch package names.  See my (arch-pkgs repo)[http://github.com/ericgebhart/arch-pkgs] for a meta package that will install most of this.

You will need:
* xmonad
* xmonad-log-applet (from my repo)[http://github.com/ericgebhart/xmonad-log-applet]
* dzen2
* dmenu
* rxvt-unicode - or change xmonad.hs to point at your favorite terminal.
* termite - or change xmonad.hs to point at your favorite terminal.
* xcompmgr or compton if you want transparency to work.
* adobe-source-code-pro-fonts
* ttf-ubuntu-font-family
* xfce4-panel
* ghc is nice to have for haskell coding. ie. if you modify xmonad.hs.
* feh        -- set background image.
* onboard    -- on screen keyboard
* easysroke  -- gesture support.

A couple of things about keys, I'm still figuring what I want, but one of the things that effects my decisions is that I use a dvorak keyboard.  I put my favorite commands on or close to the home row, which may not make sense on a qwerty keyboard. One menu that really stands out as dvorak specific is the floating window sub menu. It has commands on the right hand to place windows in 9 different places as if your right hand was on a keypad. Any time someone might be tempted to use `hjkl` as arrow keys I use `htns` which is the right hand home row.

**Some key features**  
 * EZConfig
 * topic space 
 * grid select 
   * workspaces
   * windows
   * shift
   * bring
   * scratchpads
   * Prompted Search & Selected search
   * Applications
 * Window marking/tagging.
 * prompt & select search 
 * other prompts - man prompt, shell prompt,
 * floatkeys - resize and move floating windows.
 * magnifier 
 * named scratchpads 
 * submap keymaps with key hints popup via dzen2, thank you [pclewis!](https://github.com/pclewis/dotfiles/tree/master/xmonad/.xmonad) , 
 * working screenshot
 * dmenu 
 * xfce-panel
 * cellwriter  - if you want pen gesture support.
 * emacs script to start emacs as client, or standalone, and to call an elisp function on startup. -- I use standalone emacs running mu4e for email and a separate emacs session for coding. Sometimes I want coding sessions to be homogoneous through emacs client and sometimes I want them standalone. This script takes care of that for me in a nice way.

I am not currently using this with the xfce-session manager.  I did for a while, but it seems unnecessary. I'm only using
the xfce-panel.  So somethings on the applications menu don't work. But I almost never use that menu.

See my [xmonad-log-applet](https://github.com/EricGebhart/xmonad-log-applet) repo to get xfce-panel  to display xmonad information.

Key Hints
---------
The important bits for keyhints is to put `showHintForKeymap.sh` in your .xmonad directory. If things are displayed to   tightly it may be necessary to increase the line height. 
 
If you want to change the font for dzen something like this works in .Xresources. You can also pass the font in to the showhintforkeymap.sh function.

```dzen2.font: -*-ubuntu mono-*-*-*-*-*-*-*-*-*-*-*-*```

A fixed font of some sort is necessary for this. The code below uses
the ubuntu mono font.  

**The functionality for keyhints is in this code:**

** Thank you to PCLewis for the initial version of this **
```
 -- Key Map doc ------------------------------------------------

windowScreenSize :: Window -> X (Rectangle)
windowScreenSize w = withDisplay $ \d -> do
    ws <- gets windowset
    sc <- fromMaybe (W.current ws) <$> pointScreen 10 10

    return $ screenRect . W.screenDetail $ sc
  where fi x = fromIntegral x

focusedScreenSize :: X (Rectangle)
focusedScreenSize = withWindowSet $ windowScreenSize . fromJust . W.peek

  -- withWindowSet $ \ws -> do
  -- ss <- windowScreenSize $ fromJust $ W.peek ws
  -- return ss

keyColor = "yellow"
cmdColor = "cyan"
-- double quoted so it can make it all the way to dzen.
dzenFont = "\"-*-ubuntu mono-*-*-*-*-*-*-*-*-*-*-*-*\""
lineHeight = "18"

keyMapDoc :: String -> X Handle
keyMapDoc name = do
  ss <- focusedScreenSize
  handle <- spawnPipe $ unwords ["~/.xmonad/showHintForKeymap.sh",
                                 name,
                                 show (rect_x ss),
                                 show (rect_y ss),
                                 show (rect_width ss),
                                 show (rect_height ss),
                                 keyColor,
                                 cmdColor,
                                 dzenFont,
                                 lineHeight]
  return handle

toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  pipe <- keyMapDoc name
  submap $ mkKeymap c m
  io $ hClose pipe


```

Creating a submap key menu is easy. The key hint label will be the name of the keymap variable, or the comment on the first line.
In the example below "Raise" will be the title. Without the comment "raiseKeymap" would be the title. 
If there is a comment at the end of a keymap entry that 
will be used as the descriptor in the keyhint.  Otherwise the actual command will be used.
Formatting should be nice, but the parser is fairly tolerant. It works by looking through your
xmonad.hs for the keymap name and grabbing everything from there to the closing **]**

```
mainKeymap c = mkKeymap c $ -- Main Keys
    [

    ("M4-r",          toSubmap c "raiseKeymap" raiseKeymap)

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


Install
-------
Where to put stuff. 

* .xmonad ==> ~/         
  * the usual place for xmonad.
* .xmonad.start ==> ~/   
  * a start script for startx
* xmonad.desktop ==> /usr/share/xsessions    
  * this is the entry you need for a display manager like _sddm_ or whatever you like.

Use make
--------

To install just the xmonad parts use 'make install'. To install the xsession use 'sudo make xsession'.  This does reference the xmonad.start in your home directory, so perhaps you'll want to
put that in a more generic place.  I usually just do `startx ./xmonad.start` in my _.zlogin_.

That should do it.

