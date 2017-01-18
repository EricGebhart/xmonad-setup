# xmonad-setup
my setup files for xmonad

Lots of stuff from lots of places.  I'm just hacking at haskell here. I need to take some time to learn it better.
Still this seems to be working pretty well. The keys
and commands are still in a lot of flux. But I'm slowly exploring everything I have and figuring out what I like and
don't like. The dzen key hints really helps with exploring and remembering what is there. And also finding things
that don't work quite the way they should or the way I want.

This is all pretty fresh. I've only been using xmonad 12 since July, 2016. This is my first major refactoring since
creating my initial xmonad.hs mess.

You will need:
* xmonad
* xmonad-log-applet from my repos
* dzen2
* dmenu


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
 * xfce-panel.
 * emacs script to start emacs as client, or standalone, and to call an elisp function on startup. -- I use standalone emacs running mu4e for email and a separate emacs session for coding. Sometimes I want coding sessions to be homogoneous through emacs client and sometimes I want them standalone. This script takes care of that for me in a nice way.

I am not currently using this with the xfce-session manager.  I did for a while, but it seems unnecessary. I'm only using
the xfce-panel.  So somethings on the applications menu don't work. But I almost never use that menu.

See my [xmonad-log-applet](https://github.com/EricGebhart/xmonad-log-applet) repo to get xfce-panel working with xmonad.

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


Emacs
-----

I run emacs using my emacsn script.  emacsn basically takes care of running emacs or emacsclient,
as well as evaluating an elisp function on startup. Any additional arguments like files are passed on
to emacs in front of the _--eval_. 
Use help to see more options.

* emacsn -h _=>_  Help!
* emacsn -e _=>_ emacs -title "Email" --eval "(mu4e)"
* emacsn -m "Foo" _=>_ emacs -title "Foo" --eval "(main-window)"
* emacsn -m "haskell" *.hs _=>_ emacs -title "haskell" *.hs --eval "(main-window)"
* emacsn -f foo _=>_ emacs --eval "(foo)"

Adding -c changes it from _emacs_ to _emacsclient -c_

* emacsn -m -c _=>_ emacsclient -c --eval "(main-window)"

My **main-window** function looks like this:

```
(defun main-window ()
  (interactive)
  (balance-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-below))
 ```

I used to also start a shell, but I use scratchpads for that now.

BC Scratchpad
-------------

I've been using bc for years and the first thing I want is to change the scale.
I basically put my startup stuff in _~/.bcstart.bc_ then start bc with `bc ~/.bcstart.bc`.
of course making that an alias for bc makes a lot of sense.

`alias bc="bc ~/.bcstart.bc"`

Where to put stuff.

* .xmonad ==> ~/
* .xmonad.start ==> ~/
* .bcstart.bc ==> ~/
* xmonad.desktop ==> /usr/share/xsessions
* .Xresources ==> ~/

