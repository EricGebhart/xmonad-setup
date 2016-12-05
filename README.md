# xmonad-setup
my setup files for xmonad

Lots of stuff from lots of places.  I'm just hacking at haskell here. I need to take some time to learn it better.
Still this seems to be working pretty well.  I'm still tweaking it, but it's pretty nice at this point. The keys
and commands are still in a lot of flux. But I'm slowly exploring everything I have and figuring out what I like and
don't like. The dzen key hints really helps with exploring and remembering what is there. And also finding things
that don't work quite the way they should or the way I want.

This is all pretty fresh. I've only been using xmonad 12 since July, 2016. This is my first major refactoring since
creating my initial xmonad.hs mess.

**Some key features**  
 * EZConfig
 * topic space 
 * grid select 
 * prompt & select search 
 * other prompts - man prompt, shell prompt,
 * floatkeys
 * magnifier 
 * named scratchpads 
 * submap keymaps with key hints popup via dzen2, thank you [pclewis!](https://github.com/pclewis/dotfiles/tree/master/xmonad/.xmonad) , 
 * working screenshot
 * dmenu 
 * xfce-panel.
 * emacs script to start emacs as client, or standalone, and to call an elisp function on startup. -- I use standalone emacs running mu4e for email and a separate emacs session for coding. Sometimes I want coding sessions to be homogoneous through emacs client and sometimes I want them standalone. This script takes care of that for me in a nice way.

I am not currently using this with the xfce-session manager.  I did for a while, but it seems unnecessary. I'm only using
the xfce-panel.  So somethings on the applications menu don't work. But I almost never use that menu.

Key Hints
---------
The important bits for keyhints is to put `showHintForKeymap.sh` in your .xmonad directory. If things are displayed to   tightly it may be necessary to increase the line height.  $lh at the top.
 
If you want to change the font for dzen something like this works in .Xresources.
```dzen2.font: xft:Source\ Code\ Pro:Regular:size=14```

**The functionality for keyhints is in this code:**

```
 -- Key Map doc ------------------------------------------------

windowScreenSize :: Window -> X (Rectangle)
windowScreenSize w = withDisplay $ \d -> do
    ws <- gets windowset
    wa <- io $ getWindowAttributes d w
    bw <- fi <$> asks (borderWidth . config)
    sc <- fromMaybe (W.current ws) <$> pointScreen (fi $ wa_x wa) (fi $ wa_y wa)

    return $ screenRect . W.screenDetail $ sc
  where fi x = fromIntegral x

focusedScreenSize :: X (Rectangle)
focusedScreenSize = withWindowSet $ windowScreenSize . fromJust . W.peek

  -- withWindowSet $ \ws -> do
  -- ss <- windowScreenSize $ fromJust $ W.peek ws
  -- return ss

keyColor = "yellow"
cmdColor = "cyan"

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
                                 cmdColor]
  return handle

toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  pipe <- keyMapDoc name
  submap $ mkKeymap c m
  io $ hClose pipe

```

Creating a submap key menu is easy. If there is a comment at the end of a keymap entry that 
will be used as the discriptor in the keyhint.  Otherwise the actual command will be used.
Formatting should be nice, but the parser is fairly tolerant. It works by looking through your
xmonad.hs for the keymap name and grabbing everything from there to the closing **]**

```
mainKeymap c = mkKeymap c $
[...
    , ("M4-r",          toSubmap c "raiseKeymap" raiseKeymap)
]

raiseKeymap =
    [ ("v", runOrRaiseNext "Vivaldi" (className =? "Vivaldi")) -- Vivaldi
    , ("e", raiseNext (className =? "Emacs")) -- Emacs cycle
    , ("s", runOrRaise "Slack" (className =? "Slack")) -- Slack
    ]
```





