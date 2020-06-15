-- xmonad config

-- Author: Eric Gebhart
-- http://github.com/EricGebhart
--
--
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Exit
import XMonad hiding ( (|||) )
import XMonad.Layout.LayoutCombinators
import XMonad.Config.Kde

-- For KDE
-- import XMonad.Config.Kde

-- Actions

-- import XMonad.Actions.PhysicalScreens
import XMonad.Actions.FloatKeys
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWindows

import XMonad.Actions.CycleWS      -- (16) general workspace-switching
                                   --      goodness
import XMonad.Actions.CycleRecentWS -- (17) cycle between workspaces
                                    --      in most-recently-used order
import XMonad.Actions.Warp         -- (18) warp the mouse pointer
import XMonad.Actions.Submap       -- (19) create keybinding submaps
import XMonad.Actions.Search hiding (Query, images)
import XMonad.Actions.TopicSpace
import XMonad.Actions.GridSelect
import XMonad.Actions.WithAll      -- (22) do something with all windows on a workspace
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TagWindows
import XMonad.Actions.CopyWindow(copy)
-- Hooks.
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

import XMonad.Config.Desktop

       -- Layouts
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.MultiToggle
import XMonad.Layout.Spacing
-- import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed  -- this replaces named.
import XMonad.Layout.Named
import XMonad.Layout.LayoutScreens
import XMonad.Layout.WorkspaceDir  -- (11) set working directory
import XMonad.Layout.ResizableTile -- (5)  resize non-master windows too
import XMonad.Layout.LayoutHints
import XMonad.Layout.MultiColumns
import XMonad.Layout.OneBig
import XMonad.Layout.TwoPane
import XMonad.Layout.DwmStyle
import XMonad.Layout.IM
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import qualified XMonad.Layout.Grid as G
import XMonad.Layout.GridVariants ( Grid(..) )
import XMonad.Layout.HintedTile
import XMonad.Layout.Accordion
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import qualified XMonad.Layout.Magnifier as Mag
import XMonad.Layout.LayoutModifier ( ModifiedLayout(..) )

-- Prompts ---------------------------------------------------
import XMonad.Prompt                -- (23) general prompt stuff.
import XMonad.Prompt.Man            -- (24) man page prompt
import XMonad.Prompt.AppendFile     -- (25) append stuff to my NOTES file
import XMonad.Prompt.Ssh            -- (26) ssh prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Input          -- (26) generic input prompt, used for
                                    --      making more generic search
                                    --      prompts than those in
                                    --      XMonad.Prompt.Search
import XMonad.Prompt.Workspace      -- (27) prompt for a workspace

-- Utilities
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List
import Data.Ratio ((%))
import Data.Maybe (fromMaybe, fromJust)
import Data.Char (isSpace)

import XMonad.Util.Run
import XMonad.Util.Paste
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.XSelection
import XMonad.Util.EZConfig

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import Control.Arrow hiding ((|||), (<+>))

-------------------------------------------------------------------------
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.

myModMask = mod4Mask

-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Terminal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal = "urxvt"
myTerminal2 = "termite"
myShell = "zsh"

-- startupCommands = [
--                   ,"xrandr --fbmm 286x281"
--                   ,"xcompmgr -c"
--                   ,"xsetroot -cursor_name left_ptr"
--                   ,"xrandr --output HDMI-1 --right-of DP-1"
--                   ,"~/.config/polybar/launch.sh"
--                   ,"xfce4-panel -d"
--                   ,"xfce4-power-manager"
--                   ,"/home/ben/bin/run_wallpaper.sh"
--                   ,"redshift"
--                   ,"xscreensaver"
--                   ,"feh --bg-scale /home/eric/Documents/Art/ocean_park_114.jpg"
--                   ,"easystroke"
--                   ,"onboard"
--                   ,"emacs --daemon"
--                   ,"nm-applet & ## if using trayer add: --sm-disable"
--                   ,"blueberry-tray"
--                   ]



------------------------------------------------------------------------
-- Colors and borders
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#000000" -- "#ffb6b0"
myActiveBorderColor  = "#007c7c"

myFont = "Source Code Pro"
myMonoFont = "Source Code Pro"
myfontwsize = "xft:" ++ myFont ++ ":size=16"

-- theme settings for tabs and deco layouts.
myTheme :: Theme
myTheme = def {
  fontName = "xft:" ++ myFont ++ ":pixelsize=14"
  , decoHeight = 20
  , decoWidth = 400
  , activeColor = myFocusedBorderColor
  , inactiveColor = "#262626"
  , urgentColor = "#073642"
  , activeBorderColor = myFocusedBorderColor
  , inactiveBorderColor = "#586e75"
  , urgentBorderColor = "#586e75"
  , activeTextColor = "#CEFFAC"
  , inactiveTextColor = "#839496"
  , urgentTextColor = "#dc322f"
}

-- my old tab config theme.
-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def {
  fontName = "xft:" ++ myFont ++ ":pixelsize=14",
  activeBorderColor = "#007C7C",
  activeTextColor = "#CEFFAC",
  activeColor = myFocusedBorderColor,
  inactiveBorderColor = "#7C7C7C",
  inactiveTextColor = "#EEEEEE",
  inactiveColor = "#000000"
}

-- Width of the window border in pixels.
myBorderWidth = 1


-- The command to lock the screen or show the screensaver.
myScreensaver = "/usr/bin/gnome-screensaver-command --lock"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "screenshot"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
               --
 -- Topics replaces this...
-- myWorkspaces = ["1:Code","2:Comm","3:Lang","4:Music","5:media"] ++ map show [6..9]

-- Workspaces using TopicSpace.

data TopicItem = TI { topicName :: Topic
                    , topicDir  :: String
                    , topicAct  :: X ()
                    }

myTopics :: [TopicItem]
myTopics = [ TI "main" "" (return ())
           -- ,  TI "mail" "" (spawnInTopicDir "emacsn -e")
           , TI "Web" "" (spawnInTopicDir "vivaldi-stable")
           , TI "Xournal" "Xournal" (spawnInTopicDir "xournal")
--           , TI "Yeti" "play/Yeti/yeti-stack" (spawnInTopicDir "emacsn -m Yeti")
           , TI "Code" "play" (spawnInTopicDir "emacsn -m Code")
           , TI "Arch-Setup" "Arch-Setup" (spawnInTopicDir "emacsn -m Arch-Setup")
           , TI "bgc-ui" "play/bgc-ui" (spawnInTopicDir "emacsn -m bgc-ui")
           , TI "eg.com" "play/ericgebhart.github.io" (spawnInTopicDir "emacsn -m eg.com")
           , TI "tb.com" "play/tangobreath.github.io" (spawnInTopicDir "emacsn -m tb.com")
           , TI "Elisp" "elisp" (spawnInTopicDir "emacsn -m Elisp")
           , TI "Closh" "play/closh" (spawnInTopicDir "emacsn -m closh")
           , TI "QMK" "play/qmk_firmware" (spawnInTopicDir "emacsn -m QMK keyboards/ergodox_ez/keymaps/ericgebhart/keymap.c")
           , TI "XMonad" ".xmonad" (spawnInTopicDir "emacsn -m Xmonad xmonad.hs") -- lib/*/*.hs
           , TI "Comm" "" (spawnInTopicDir "slack" >>
                            spawnInTopicDir "emacsn -e")

           , TI "BD" "BD" (spawnInTopicDir "termite -T BD" >>
                            spawnInTopicDir "YACReaderLibrary")
           , TI "DownLoads" "Downloads" (spawnInTopicDir "termite -T Downloads" >>
                                          spawnInTopicDir "dolphin --select ~/Downloads")
           , TI "French" "Language/Française" (spawnInTopicDir "termite -T Française" >>
                                               spawnInTopicDir "dolphin --select ~/Language/Française" >>
                                               spawn "anki")
           , TI "3D" "Projects/3d" (spawnInTopicDir "repetierHost" >>
                           spawnInTopicDir "openscad" >>
                           spawnInTopicDir "emacsn -m 3D")
           , TI "Music"  "Music" (spawn "mediacenter22")
           , TI "Movies" "Movies" (spawn "vlc")
             -- , TI "calendar" "" (spawn "vivaldi --app='http://calendar.google.com'")
           , TI "Krita" "Drawings" (spawnInTopicDir "krita") -- lib/*/*.hs
           , TI "Gravit" "Drawings" (spawnInTopicDir "GravitDesigner.AppImage") -- lib/*/*.hs
             --, TI "feeds"  "" (spawn "chromium-browser --app='https://feedbin.me'")
           --, TI "stats"  "" (spawnInTopicDir "termite -e htop")
           ]

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

myTopicConfig :: TopicConfig
myTopicConfig = TopicConfig
    { topicDirs = M.fromList $ map (topicName &&& topicDir) myTopics
    , defaultTopicAction = const $ return ()
    , defaultTopic = "main"
    , maxTopicHistory = 10
    , topicActions = M.fromList $ map (topicName &&& topicAct) myTopics
    }



-- --- Prompted workspace navigation. ---------------------------------

spawnInTopicDir act = currentTopicDir myTopicConfig >>= spawnIn act

-- spawnShell :: X ()
-- spawnShell =  asks (terminal . config) >>= spawnInTopicDir

spawnShellIn :: Dir -> X ()
spawnShellIn dir = asks (terminal . config) >>= flip spawnIn dir

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

-- spawnShellIn :: Dir -> X ()
-- spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"

spawnIn act dir = spawn $ "cd " ++ dir ++ "; " ++ act

wsgrid = withWindowSet $ \w -> do
    let wss = W.workspaces w
        usednames = map W.tag $  wss
        newnames = filter (\used -> (show used `notElem` (map show myTopicNames))) usednames
    gridselect gsConfig (map (\x -> (x,x)) (myTopicNames ++ newnames))

      -- gridselect a workspace and view it
promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)
-- gridselect a workspace to shift active window to
promptedShift = wsgrid >>= flip whenJust (windows . W.shift)

-- --- Choose your method:  prompts or Grid select.  - These are prompts.
-- goto :: Topic -> X ()
-- goto = switchTopic myTopicConfig


-- promptedShift :: X ()
-- promptedShift = workspacePrompt myXPConfig $ windows . W.shift

--------------------------------------------------------------------------------


-- pretty communication with the the dbus. ie. xfce-panel.
-- I use a completely transparent panel for this. The background image
-- has a nice multi-colored bar across the top of it. - oceanpark114.jpg
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html
myPPPolybar :: D.Client -> PP
myPPPolybar dbus = def
    { ppOutput   = dbusOutput dbus
    , ppCurrent  = wrap ("%{F" ++ blue2 ++ "} ") " %{F-}"
    , ppVisible  = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent   = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    --, ppHidden = wrap " " " "
    , ppLayout   = id
    , ppHidden          = id . noScratchPad
    , ppWsSep    = " "
    , ppSep      = " | "
    --, ppTitle = myAddSpaces 25
    , ppTitle           = shorten 80
    }
  where noScratchPad ws = if ws == "NSP" then "" else ws

prettyPrinter :: D.Client -> PP

prettyPrinter dbus = def  --defaultPP
    { ppCurrent         = pangoColor "darkgreen" .wrap "[" "]" . pangoSanitize
    --, ppVisible         = wrap "<" ">"
--    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden          = id . noScratchPad
--    , ppHiddenNoWindows = noScratchPad
    , ppUrgent          = id
    , ppSep             = "   :   "
    , ppWsSep           = "   "
    , ppTitle           = shorten 80
    , ppLayout          = id
    , ppOrder           = id
    , ppOutput          = dbusOutput dbus -- putStrLn
--  , ppSort            = getSortByIndex
    , ppExtras          = []
    }
  where noScratchPad ws = if ws == "NSP" then "" else ws

spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
  spawn program
  windows $ W.greedyView workspace

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log")
                  (D.interfaceName_ "org.xmonad.Log")
                  (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<span size=\"small\"><b>" ++ (UTF8.decodeString str) ++ "</b></span>")]
            }
    D.emit dbus signal


pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\" size=\"small\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs


-- Scratch Pads ------------------------------------------------------------
-- location and dimension.
scratchpadSize = W.RationalRect (1/4) (1/4) (1/3) (3/7)

mySPFloat = customFloating scratchpadSize

            -- with a flexible location.
-- Big BSP, Small SSP, Super small,
--  so size is width and height. - change the fractions to get your sizes right.
flexScratchpadSize dx dy = W.RationalRect (dx) (dy) (1/2) (5/7)
flexSScratchpadSize dx dy = W.RationalRect (dx) (dy) (3/5) (5/8)
flexSSScratchpadSize dx dy = W.RationalRect (dx) (dy) (1/2) (1/2)
-- pass in a fraction to determine your x,y location. size is derived from that
-- all based on screen size.
flexFloatSSP dx dy = customFloating (flexSScratchpadSize dx dy)
flexFloatSSSP dx dy = customFloating (flexSSScratchpadSize dx dy)
flexFloatBSP dx dy = customFloating (flexScratchpadSize dx dy)

scratchpads =
  [ NS "term"  (myTerminal2 ++ " -t term") (title =? "term") (flexFloatBSP (1/20) (1/20))
  , NS "term2" (myTerminal2 ++ " -t term2") (title =? "term2") (flexFloatBSP (2/20) (2/20))
  , NS "term3" (myTerminal2 ++ " -t term3") (title =? "term3") (flexFloatBSP (4/20) (4/20))
  , NS "term4" (myTerminal2 ++ " -t term4") (title =? "term4") (flexFloatBSP (6/20) (4/20))
  , NS "ghci"  (myTerminal2 ++ " -e ghci") (title =? "ghci") (flexFloatBSP (6/20) (1/10))
  --, NS "sync"  (myTerminal ++ " -e sy") (title =? "sy") (flexFloatSP (1/10) (2/3))
  , NS "top"   (myTerminal2 ++ " -e htop") (title =? "htop") (flexFloatSSP (1/4) (1/4))
  , NS "calc"  (myTerminal2 ++ " -e bcl -t bc") (title =? "bc") (flexFloatSSSP (1/4) (1/4))
  --, NS "OSX"   "vboxmanage startvm El Capitan" (title =? "El Capitan") (flexFloatSP (2/3) (2/3))
  --, NS "MSW"   "vboxmanage startvm Windows" (title =? "Windows") (flexFloatSP (2/3) (2/3))
  ]

-- This is how to make a runSelectedAction grid select menu.
-- A grid select for scratchpads.
myScratchpadMenu =
  [ ("Term1", (scratchToggle "term"))
  , ("Term2", (scratchToggle "term2"))
  , ("Term3", (scratchToggle "term3"))
  , ("Term4", (scratchToggle "term4"))
  , ("ghci",  (scratchToggle "ghci"))
  , ("top",   (scratchToggle "top"))
  --, ("sync",  (scratchToggle "sync"))
  , ("calc",  (scratchToggle "calc"))
  --, ("OSX",   (scratchToggle "OSX"))
  --, ("MSW",   (scratchToggle "MSW"))
  , ("Scratch", scratchpadSpawnActionTerminal  "urxvt")
  -- , ("Scratch", scratchpadSpawnActionTerminal  "urxvt -background rgba:0000/0000/0200/c800")
  ]

--- grid select for some apps.
myApps = [("Terminal",     (spawn     myTerminal))

         ,("Vivaldi",      (runOrRaiseNext  "vivaldi" (className =? "vivaldi")))
         -- ,("Firefox",      (raiseApp  "fox" "firefox"))
         -- ,("Chromium",     (raiseApp  "web" "chromium"))

         ,("Emacs",        (spawn "emacsn -m Code"))
         -- ,("Steam",        (raiseApp  "steam" "steam"))
         ,("Gimp",         (runOrRaise "gimp" (className =? "gimp")))
         ,("Krita",         (runOrRaise "krita" (className =? "krita")))
         -- ,("Win7",         (raiseApp  "Win7" "virtualbox --startvm Win7 --start-paused"))
         -- ,("Inkscape",     (raiseApp  "ink" "inkscape"))

         ,("Dolphin",      (runOrRaise "dolphin" (className =? "dolphin")))
         ,("LibreOffice",  (runOrRaise "libreoffice" (className =? "libreoffice")))

         ,("Video",        (runOrRaise "vlc" (className =? "vlc")))

         ]


  -- where
  --   raiseApp ws a = (raiseNextMaybe (spawnWS ws a) (appName ~? a)) >> bringMouse
  --   raiseApp' a = (raiseNextMaybe (spawn a) (appName ~? a)) >> bringMouse
  --   --raiseClass ws a c = (raiseNextMaybe (spawnWS ws a) (className ~? c)) >> bringMouse
  --   --raiseClass' a c = (raiseNextMaybe (spawn a) (className ~? c)) >> bringMouse
  --   --gksuApp ws a = (raiseNextMaybe (spawnWS ws ("gksudo " ++ a)) (appName ~? a)) >> bringMouse
  --   --myRaiseTerm a d = (raiseNextMaybe (spawnWS a (termApp a d)) (role ~? a)) >> bringMouse
  --   --termApp a d = myTerm ++ " -r " ++ a ++ " --working-dir=" ++ d ++ " -l " ++ a


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHelpers = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- classFloats]
    , [ title       =? t --> doFloat           | t <- titleFloats]
    , [ resource    =? r --> doFloat           | r <- resourceFloats]
    , [ title       =? c --> doIgnore          | c <- titleIgnores]
    , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
--    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
--    , [ className   =? c --> doF (W.shift "3") | c <- ircApps]
    ]
  where classFloats    = ["Galculator", "Steam", "Media_Center_26", "MPlayer", "Gimp", "Gajim.py", "Xmessage"]
        titleFloats    = ["alsamixer", "Onboard"]
        resourceFloats = ["desktop_window", "Dialog", "gpicview"]
        titleIgnores   = ["stalonetray", "xfce4-notifyd"]
--        webApps      = ["Firefox-bin", "Opera"] -- open on desktop 2
--        ircApps      = ["Ksirc"]                -- open on desktop 3

-- From I3 with kde config
-- for_window [class="plasmashell"] floating enable
-- for_window [class="Plasma"] floating enable
-- for_window [title="plasma-desktop"] floating enable
-- for_window [class="Plasmoidviewer"] floating enable

-- # Float by type
-- for_window [window_role="pop-up"] floating enable
-- for_window [window_role="task_dialog"] floating enable
-- for_window [window_type="dialog"] floating enable
-- for_window [window_type="menu"] floating enable

-- myManageHelpers = composeAll
--     [
--     resource  =? "desktop_window" --> doIgnore
--     --, className =? "Chromium"       --> doShift "2:Comm"
--     --, className =? "Google-chrome"  --> doShift "comm"
--     , className =? "Galculator"     --> doFloat
--     , className =? "Steam"          --> doFloat
--     , className =? "Gimp"           --> doFloat
--     , appName =? "JRiver Popup Class"         --> doFloat
--     , className =? "Media_Center_26"         --> doFloat
--     , resource  =? "gpicview"       --> doFloat
--     , className =? "MPlayer"        --> doFloat
--     , className =? "vivaldi"        --> doShift "comm"
--     , className =? "VirtualBox"     --> doFloat
--     , title     =? "Onboard"        --> doFloat
--       --, className =? "VirtualBox"     --> doShift "3:Lang"
--     --, className =? "anki"           --> doShift "3:Lang"
--     --, title     =? "Anki"           --> doShift "3:Lang"
--     --, className =? "Xchat"          --> doShift "5:media"
--     , className =? "stalonetray"    --> doIgnore
--     , className =? "stalonetray"    --> doIgnore
--     , className =? "xfce4-notifyd"  --> doIgnore
--     , isFullscreen --> (doF W.focusDown <+> doFullFloat)
--     , resource =? "Dialog" --> doFloat
--       --, isDialog -?> doFloat
--       -- , isFullscreen --> doFullFloat
--   ]

-- -- move and resize on float.  what size and where ?
-- -- Set x, y, gx1, gy1, dx, dy, gx2 and gy2 accordingly.
-- -- or sink it if it's already floating.
-- toggleFloat = withFocused (\windowId -> do
--                               { floats <- gets (W.floating . windowset);
--                                 if windowId `M.member` floats
--                                 then withFocused $ windows . W.sink
--                                 else do
--                                      keysMoveWindowTo (x, y) (gx1, gy1) windowId
--                                      keysResizeWindow (dx, dy) (gx2, gy2) windowId
--                               }
--                           )


manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.6
    w = 0.5
    t = 1 - h
    l = 1 - w

myManageHook = myManageHelpers <+>
    manageScratchPad


------------------------------------------------------------------------
-- Layouts
-- Per workspace layouts. without multi-toggle.
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- perworkspace layouts....
 -- layoutH = layoutHints . smartBorders
 --        $ onWorkspace "music"   (tiled
 --                                ||| Mirror tiled
 --                                ||| Full
 --                                )
 --        $ onWorkspace "chat"    (tiled
 --                                ||| Mirror tiled
 --                                ||| Full
 --                                )
 --        $ onWorkspace "web"     (Mirror tiled2
 --                                ||| tiled2
 --                                ||| Full
 --                                )
 --        $ Mirror tiled
 --            ||| tiled
 --            ||| Full
 --            ||| dragPane Horizontal 0.1 0.5
 --  where
 --     tiled  = Tall 1 (3 % 100) (3 % 5)
 --     tiled2 = Tall 1 (3 % 100) (4 % 5)

-- myLayouts = avoidStruts $ smartBorders $
    --   onWorkspace "2:im" (named "IM" (reflectHoriz $ withIM (1%8) (Title "Buddy List") (reflectHoriz $ dwmStyle shrinkText myTheme tiled ||| (smartBorders $ tabs)))) $
--   onWorkspace "3:web" (tabs) $
--   (tiled ||| named "Mirror" (Mirror tiled) ||| tabs)
--     where
--       tiled = named "Tall" (ResizableTall 1 (3/100) (1/2) [])
--       tabs = named "Tabs" (tabbed shrinkText myTheme)

-- -- this is one of the layouts provided by grid variants
-- SplitGrid L 2 3 (2/3) (16/10) (5/100)
-- ((modm .|. shiftMask, xK_equal), sendMessage $ IncMasterCols 1),
--  ((modm .|. shiftMask, xK_minus), sendMessage $ IncMasterCols (-1)),
--  ((modm .|. controlMask,  xK_equal), sendMessage $ IncMasterRows 1),
--  ((modm .|. controlMask,  xK_minus), sendMessage $ IncMasterRows (-1))
-- ------------------------------------------------------------------------
-- -- Layouts:

-- this is so we can have layouts with multi-toggle.

-- data MyTransformers = SIDEBAR
  --                     | MAG
  --                     | RFULL
  --                     | FULL
  --                     deriving (Read, Show, Eq, Typeable)

-- instance Transformer MyTransformers Window where
  --   transform SIDEBAR x k = k (withIM (1/5) (Const True) x) (\(ModifiedLayout _ x') -> x')
  --   transform MAG x k = k (Mag.magnifiercz 1.2 x) (\(ModifiedLayout _ x') -> x')
  --   transform RFULL x k = k (avoidStrutsOn [] $ noBorders Full) (const x)
  --   -- I'm sure I was doing something wrong that caused me to need this.
  --   transform FULL x k = k (Full) (const x)

  -- -- Change LayoutHintsToCenter to LayoutHints if you like gaps between your windows.
  -- myLayout = configurableNavigation (navigateColor myActiveBorderColor)
  --          $ mkToggle (single RFULL)
  --          $ avoidStruts
  --          $ mkToggle (single MAG)
  --          $ mkToggle (single FULL)
  --          $ (onWorkspace "gimp" $ named "gimp" $ withIM (2/11) (Role "gimp-toolbox") $ big')
  --          $ mkToggle (single SIDEBAR)
  --          $ layouts
  -- where
  --   layouts = tall' ||| cols' ||| twopane' ||| rows' ||| tabs'
  --       ||| grid' ||| big' ||| circle' ||| bsp' ||| accordion' ||| noborders'
  --   tall'    = named "tall"   $ layoutHintsToCenter $ XMonad.Tall 1 (3/100) (1/2)
  --   cols'    = named "cols"   $ layoutHintsToCenter $ deco $ multiCol [1] 2 (3/100) (1/2)
  --   twopane' = named "two"    $ layoutHintsToCenter $ TwoPane (3/100) (3/7)
  --   rows'    = named "rows"   $ Mirror $ layoutHintsToCenter $ deco $ multiCol [2] 3 (2/100) (4/7)
  --   tabs'    = named "tab"    $ layoutHintsToCenter $ tabs
  --   grid'    = named "grid"   $ layoutHintsToCenter $ deco $ Grid (16/10)
  --   big'     = named "big"    $ layoutHintsToCenter $ deco $ Mirror $ OneBig (3/4) (19/24)
    --   circle'  = named "circle" $ layoutHintsToCenter $ Circle
    --   bsp'     = named "BSP"    $ layoutHintsToCenter $ BSP.emptyBSP
    --   accordion' = named "accordion" $ layoutHintsToCenter $ mirrorAccordion
    --   noborders' = named "noborders" $ layoutHintsToCenter $ noBorders (fullscreenFull Full)
    --   -- basic layouts
    --   tabs     = tabbed shrinkText myTheme
    --   deco     = dwmStyle shrinkText myTheme
    --   mirrorAccordion = Mirror (Accordion)
    --   -- property query
    --   role = stringProperty "WM_WINDOW_ROLE"

  -- My Original layouts...
myLayout = avoidStruts (
  ThreeColMid 1 (3/100) (1/2) |||
    XMonad.Tall 1 (3/100) (1/2) |||
    Mirror (XMonad.Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    TwoPane (3/100) (1/2) |||
    Mag.magnifier BSP.emptyBSP |||
    Circle |||
    Mag.magnifier tiled ||| hintedTile XMonad.Layout.HintedTile.Tall ||| hintedTile Wide |||
    Accordion |||
    mirrorAccordion |||
    Grid (8/4) |||
    -- spiral (6/7)
    noBorders (fullscreenFull Full))
  where
    mirrorAccordion = Mirror (Accordion)
    tiled = XMonad.Tall tnmaster tdelta ratio
    tnmaster = 1
    ratio = 1/2
    tdelta = 3/100
    hintedTile = HintedTile hnmaster hdelta hratio TopLeft
    hnmaster = 1
    hratio = 1/2
    hdelta = 3/100

-- layout prompt (w/ auto-completion and all layouts)
myLayoutPrompt = inputPromptWithCompl mylayoutXPConfig "Layout"
                    (mkComplFunFromList' allLayouts)
                    ?+ (sendMessage . JumpToLayout)
mylayoutXPConfig = def { autoComplete = Just 1000 }
allLayouts = ["tall", "wide", "circle", "full", "tabbed", "accordion"]

-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' = cycleWindowSets options
 where options w = map (W.view `flip` w) (recentTags w)
       recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]

-- Warp
bringMouse = warpToWindow (9/10) (9/10)

-- Scratchpad invocation (for brevity)
scratchToggle a = namedScratchpadAction scratchpads a >> bringMouse

mypromptSearch a = promptSearch myXPConfig a

isGDHist a = (a == "GoldenDict:")

gDPrompt :: XPConfig -> X ()
gDPrompt c =
    inputPromptWithCompl c "GoldenDict:" (historyCompletionP isGDHist) ?+ \word ->
-- -- this causes the full sized goldendict to appear.
-- runProccessWithInput "goldendict" [] word
    safeSpawn "goldendict" [word]
         >> return ()

-- these don't actually work for some reason. The data scrolls out of the dzen bar.
calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace

sdcvPrompt :: XPConfig -> String -> X ()
sdcvPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "sdcv" ["-2 /SSD/usr/share/startdict"] input) >>= sdcvPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace

-- Extra search engines for promptsearch and select search
-- Search Perseus for ancient Greek dictionary entries
greek     = searchEngine "greek"      "http://www.perseus.tufts.edu/hopper/morph?la=greek&l="
images    = searchEngine "images"     "http://www.google.com/search?hl=fr&tbm=isch&q="
reverso   = searchEngine "reverso"    "http://context.reverso.net/traduction/francais-anglais/"
arch      = searchEngine "arch"       "http://wiki.archlinux.org/index.php/Special:Search?search="
archpkgs  = searchEngine "archpkgs"   "https://www.archlinux.org/packages/?sort=&q="
archaur   = searchEngine "archaur"    "https://aur.archlinux.org/packages/?O=0&K="
thesaurus = searchEngine "thesaurus"  "http://thesaurus.reference.com/browse/"
etymology = searchEngine "etymology"  "http://www.etymonline.com/index.php?term="
synonyms  = searchEngine "synonyms"   "http://www.synonymes.com/synonyme.php?mot="
-- synonym  = searchEngine "synonymes" "http://www.les-synonymes.com/mot/"
wiktionnaire = searchEngine "wiktionnaire" "https://fr.wiktionary.org/w/index.php?search="
clojuredocs = searchEngine "clojuredocs" "https://clojuredocs.org/clojure.core/"

promptSearchMenu =
     [ ("man",          (manPrompt myXPConfig))
--     , ("sdcv",         (sdcvPrompt myXPConfig "sdcv"))
--     , ("calc",         (calcPrompt myXPConfig "calc"))
     , ("goldendict",   (gDPrompt myXPConfig))
     , ("google",       (mypromptSearch google))
     , ("hoogle",       (mypromptSearch hoogle))
     , ("clojuredocs",  (mypromptSearch clojuredocs))
     , ("duckduckgo",   (mypromptSearch duckduckgo))
     , ("wikipedia",    (mypromptSearch wikipedia))
     , ("hackage",      (mypromptSearch hackage))
     , ("scholar",      (mypromptSearch scholar))
     , ("math World",   (mypromptSearch mathworld))
     , ("Maps",         (mypromptSearch maps))
     , ("Dictionary",   (mypromptSearch dictionary))
     , ("Alpha",        (mypromptSearch alpha))
     , ("Lucky",        (mypromptSearch lucky))
     , ("Images",       (mypromptSearch images))
     , ("greek",        (mypromptSearch greek))
     , ("Reverso",      (mypromptSearch reverso))
     , ("Arch",         (mypromptSearch arch))
     , ("Arch Pkg",     (mypromptSearch archpkgs))
     , ("Arch AUR",     (mypromptSearch archaur))
     , ("Wiktionnaire", (mypromptSearch wiktionnaire))
     , ("Synonymes.fr", (mypromptSearch synonyms))
     ]

selectSearchMenu =
     [ ("google",       (selectSearch google))
     , ("goldendict",   (promptSelection "goldendict"))
     , ("hoogle",       (selectSearch hoogle))
     , ("clojuredocs",  (selectSearch clojuredocs))
     , ("duckduckgo",   (selectSearch duckduckgo))
     , ("wikipedia",    (selectSearch wikipedia))
     , ("hackage",      (selectSearch hackage))
     , ("scholar",      (selectSearch scholar))
     , ("math World",   (selectSearch mathworld))
     , ("Maps",         (selectSearch maps))
     , ("Dictionary",   (selectSearch dictionary))
     , ("Alpha",        (selectSearch alpha))
     , ("Lucky",        (selectSearch lucky))
     , ("Images",       (selectSearch images))
     , ("greek",        (selectSearch greek))
     , ("Reverso",      (selectSearch reverso))
     , ("Arch",         (selectSearch arch))
     , ("Arch Pkg",     (selectSearch archpkgs))
     , ("Arch AUR",     (selectSearch archaur))
     , ("Wiktionnaire", (selectSearch wiktionnaire))
     , ("Synonymes.fr", (selectSearch synonyms))
     ]

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = def --  defaultXPConfig                            -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    , font = "xft:Source Code Pro:size=14"
    , height = 96
    }

crizer :: String -> Bool -> X(String, String)
crizer _ False = return ("#002b36", "#839496")
crizer _ True = return ("#839596", "#002b36")

{-
crizer :: String -> Bool -> X(String, String)
crizer _ False = return ("#fdf6e3", "#657b83")
crizer _ True = return ("#657b83", "#fdf6e3")
-}

gsConfig = def {   -- defaultGSConfig
           gs_colorizer = crizer
           ,gs_cellheight  = 150
           ,gs_cellpadding = 5
           ,gs_cellwidth   = 400
           ,  gs_font = "xft:Source Code Pro:pixelsize=48"
           }

-- I don't know why, but gotoSelected like
-- ,bgColor = "#3f6644"
gsConfig2 = def {
                gs_cellheight = 75
                ,gs_cellpadding = 5
                ,gs_cellwidth = 600
                , gs_font = "xft:Source Code Pro:pixelsize=36"
                }

myBack    = "#1a1a1a" -- Bar background
myFore    = "#999999" -- Bar foreground
myAcc     = "#25629f" -- Accent color
myHigh    = "#629f25" -- Highlight color
myLow     = "#000000" -- Lowlight color
myVis     = "#9f2562" -- Visible Workspace
myEmpt    = "#555555" -- Empty workspace

-- GridSelect config
myGSConfig colorizer = (buildDefaultGSConfig colorizer)
                       {gs_cellheight  = 95
                       ,gs_cellpadding = 5
                       ,gs_cellwidth   = 250
                       , gs_font = "xft:Source Code Pro:pixelsize=40"
                       }

-- Colorizer colors for GridSelect
--aqua   = myColor "#259f62"
blue   = myColor "#25629f"
green  = myColor "#629f25"

-- orange = myColor "#9f6225"
-- pink   = myColor "#9f2562"
-- purple = myColor "#62259f"

-- Colorizer generator
myColor color _ isFg = do
  return $ if isFg
           then (color, myLow)
           else (myLow ,color)

warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5
selectApps   = runSelectedAction (myGSConfig green) myApps

getScratchpad = runSelectedAction (myGSConfig blue) myScratchpadMenu
searchStuff = runSelectedAction (myGSConfig green) promptSearchMenu
selectSearchStuff = runSelectedAction (myGSConfig green) promptSearchMenu

 -- Key Map doc dzen popup ------------------------------------------------

windowScreenSize :: Window -> X (Rectangle)
windowScreenSize w = withDisplay $ \d -> do
    ws <- gets windowset
    sc <- fromMaybe (W.current ws) <$> pointScreen 10 10 -- who cares where.

    return $ screenRect . W.screenDetail $ sc
  where fi x = fromIntegral x

focusedScreenSize :: X (Rectangle)
focusedScreenSize =
  withWindowSet $ windowScreenSize . fromJust . W.peek

  -- withWindowSet $ \ws -> do
  -- ss <- windowScreenSize $ fromJust $ W.peek ws
  -- return ss

keyColor = "yellow"
cmdColor = "cyan"
-- double double quoted so it can make it all the way to dzen.
dzenFont = "\"-*-ubuntu mono-*-*-*-*-*-96-*-*-*-*-*-*\""
lineHeight = "24"

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

    ------------------------------------------------------------------------
-- Key bindings
--
--
-- Note: Formatting is important for script
focusKeymap = -- Focus
  [ ("v",       focus "vivaldi")
  , ("e",       focus "emacs")
  , ("m",       windows W.focusMaster) -- Focus Master
  , ("s",       windows W.swapMaster) -- Swap Master
  , ("/",       spawn menu) -- Menu
  , ("t",       withFocused $ windows . W.sink) -- Sink
  , ("<Up>",    windows W.swapUp) -- Swap Up
  , ("<Down>",  windows W.swapDown) -- Swap Down
  , ("z",       rotOpposite) -- Rotate Opposite
  , ("i",       rotUnfocusedUp) -- Rotate UnFocused UP
  , ("d",       rotUnfocusedDown) -- Rotate Focused Down
  , ("r",       refresh)
  , ("<Right>", sendMessage MirrorExpand) -- Mirror Expand
  , ("<Left>",  sendMessage MirrorShrink) -- Mirror Shrink
  , ("n",       shiftToNext) -- -> Next
  , ("p",       shiftToPrev) -- -> Prev
  , ("S-n",     shiftToNext >> nextWS) -- -> Next & follow
  , ("S-p",     shiftToPrev >> prevWS) -- -> Prev & follow
  ]
  where focus :: String -> X ()
        focus w = spawn ("wmctrl -a " ++ w)
        menu = "wmctrl -l | cut -d' ' -f 5- | sort | uniq -u | dmenu -i | xargs -IWIN wmctrl -F -a WIN"

musicKeymap = -- Music
  [ ("n", mpc "next") -- Next
  , ("N", mpc "prev") -- Prev
  , ("p", mpc "toggle") -- Toggle
  , ("r", mpc "random") -- Random
  , ("l", mpc "repeat") -- Repeat
  ]
  where mpc c = spawn ("mpc " ++ c)

masterKeymap = -- Master Window
  [ ("f",      windows W.focusMaster) -- Focus
  , ("s",      windows W.swapMaster) -- Swap
  , ("h",      sendMessage Shrink) -- Shrink
  , ("l",      sendMessage Expand) -- Expand
  , ("k",      incMaster) -- Master++
  , ("j",      decMaster) -- Master--
  , ("<Up>",   incMaster) -- Master++
  , ("<Down>", decMaster) -- Master--
  ]
  where incMaster       = sendMessage (IncMasterN 1)
        decMaster       = sendMessage (IncMasterN (-1))


shotKeymap = -- Screen Shot
  [ ("c", setContext) -- Set Context
  , ("s", takeShot select) -- Take Select
  , ("w", takeShot currentWindow) -- Take Current Window
  , ("o", openDirectory) -- Open Directory
  ]
  where setContext = spawn ("~/.xmonad/sshot-context.sh")
        takeShot a = spawn ("scrot " ++ a ++ " ~/screenshots/current-context/'%Y-%m-%dT%H%M%S_$wx$h.png'")
        openDirectory = spawn ("xdg-open ~/screenshots/current-context/")
        select        = "-s"
        currentWindow = "-u"

-- Make sure you have $BROWSER set in your environment.
promptSearchKeymap = -- Search
     [ ("m", manPrompt myXPConfig) -- Man Pages
     , ("f", prompt "goldendict" myXPConfig) -- golden dict
     , ("g", promptSearch myXPConfig google) -- Google
     , ("d", promptSearch myXPConfig duckduckgo) -- duck duck go
     , ("w", promptSearch myXPConfig wikipedia) -- wikipedia
     , ("h", promptSearch myXPConfig hackage) -- hackage
     , ("s", promptSearch myXPConfig scholar) -- Scholar
     , ("S-m", promptSearch myXPConfig mathworld) -- Math World
     , ("c", promptSearch myXPConfig maps) -- Maps / Cartes
     , ("S-d", promptSearch myXPConfig dictionary) -- Dictionary
     , ("a", promptSearch myXPConfig alpha) -- Alpha
     , ("l", promptSearch myXPConfig lucky) -- Lucky
     , ("i", promptSearch myXPConfig images) -- Images
     , ("k", promptSearch myXPConfig greek) -- Greek
     , ("r", promptSearch myXPConfig reverso) -- Reverso
     ]

selectSearchKeymap = -- Search Selected
    [ ("g", selectSearch google) -- Google
    , ("f", promptSelection "goldendict") -- golden dict
    , ("d", selectSearch duckduckgo) -- Duckduckgo
    , ("w", selectSearch wikipedia) -- Wikipedia
    , ("h", selectSearch hackage) -- hackage
    , ("s", selectSearch scholar) -- Scholar
    , ("m", selectSearch mathworld) -- Mathworld
    , ("c", selectSearch maps) -- Maps / Cartes
    , ("S-d", selectSearch dictionary) -- Dictionary
    , ("a", selectSearch alpha) -- Alpha
    , ("l", selectSearch lucky) -- Lucky
    , ("i", selectSearch images) -- Images
    , ("k", selectSearch greek) -- Greek
    , ("r", selectSearch reverso) -- Reverso
    ]


 -- some prompts.
 -- ability to change the working dir for a workspace.
promptsKeymap = -- Prompts
        [ ("d",   changeDir myXPConfig) -- Change Dir
        , ("m",   manPrompt myXPConfig) -- Man Pages
        , ("r",   spawn "exe=`dmenu_run -fn myfontwize -b -nb black -nf yellow -sf yellow` && eval \"exec $exe\"") -- dmenu
        , ("n",   appendFilePrompt myXPConfig "$HOME/NOTES") -- append notes
        , ("S-s", sshPrompt myXPConfig) -- SSH
        , ("z",   shellPrompt myXPConfig) -- Shell
        , ("s",   promptSearch myXPConfig $ intelligent multi) -- Multi Search
        ]

        -- , ("e", spawn "exe=`echo | yeganesh -x` && eval \"exec $exe\"")

namedScratchpadsKeymap = -- Scratch Pads
    [ ("o", scratchToggle "term") -- Term
    , ("e", scratchToggle "term2") -- Term2
    , ("u", scratchToggle "term3") -- Term3
    , ("h", scratchToggle "term4") -- Term4
    , ("g", scratchToggle "ghci") -- ghci
    , ("c", scratchToggle "calc") -- calc
    , ("t", scratchToggle "top") -- top
    , ("n", scratchpadSpawnActionTerminal "urxvt -background rgba:0000/0000/0200/c800") -- scratchpad
    , ("S-o", scratchToggle "OSX") -- OS X
    , ("w", scratchToggle "MSW") -- MS Windows
    ]

xfceKeymap = -- XFCE
    [ ("M4-n", spawnShell) -- Terminal
    , ("q",    spawn "fce4-session-logout")
    , ("S-p",  spawn "xfce4-appfinder")
    , ("q",    spawn "fce4-session-logout")
    ]

magnifierKeymap = -- Magnifier
    [ ("+",   sendMessage Mag.MagnifyMore)  -- More
    , ("-",   sendMessage Mag.MagnifyLess)  -- Less
    , ("o",   sendMessage Mag.ToggleOff  )  -- Off
    , ("S-o", sendMessage Mag.ToggleOn   )  -- On
    , ("m",   sendMessage Mag.Toggle     )  -- Toggle
    ]

workspacesKeymap = -- Workspaces
    [ ("z",      toggleWS) -- toggle
    , ("n",      nextWS) -- Next
    , ("p",      prevWS) -- prev
    , ("<Up>",   nextScreen) -- next Screen
    , ("<Down>", prevScreen) -- prev Screen
    , ("S-n",    shiftToNext) -- Shift -> next
    , ("S-p",    shiftToPrev) -- shift -> prev
    , ("C-n",    shiftToNext >> nextWS) -- Shift -> Next & follow
    , ("C-p",    shiftToPrev >> prevWS) -- Shift -> Prev & follow
    , ("<Tab>",  cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave) -- Cycle Recent
    , ("S-z",    killAll >> DO.moveTo Prev HiddenNonEmptyWS) -- Kill All
    , ("a",      addWorkspacePrompt myXPConfig) -- Add
    , ("r",      removeEmptyWorkspace) -- Remove
    , ("s",      selectWorkspace myXPConfig) -- Select
    , ("S-r",    renameWorkspace myXPConfig) -- Rename
    , ("C-<R>",  DO.swapWith Next NonEmptyWS) -- Swap Next
    , ("C-<L>",  DO.swapWith Prev NonEmptyWS) -- Swap Prev
    , ("S-<R>",  DO.shiftTo Next HiddenNonEmptyWS) -- Shift to Next
    , ("S-<L>",  DO.shiftTo Prev HiddenNonEmptyWS) -- Shift to Prev
    , ("<R>",    DO.moveTo Next HiddenNonEmptyWS) -- Move to Next
    , ("<L>",    DO.moveTo Prev HiddenNonEmptyWS) -- Move to Prev
    ]

layoutKeymap = -- Layout
    [
--("f",   sendMessage (Toggle FULL)) --toggle Full
--, ("s",   sendMessage (Toggle SIDEBAR)) -- toggle sidebar
--, ("M-d", sendMessage (Toggle MAG)) -- toggle mag
--, ("S-f", sendMessage (Toggle RFULL)) -- Full without panel, border
      ("t",   withFocused $ windows . W.sink) -- sink focused window
    , ("S-t", sinkAll) -- sink all windows
    , ("r",   rescreen) -- Rescreen
    , ("2",   layoutSplitScreen 2 $ TwoPane (3/100) (1/2)) -- Split Screen two pane
    , ("3",   layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2)) -- Split Screen 3 Col
    , ("4",   layoutSplitScreen 4 $ G.Grid) -- Split Screen Grid
    ]

floatKeymap = -- Floating Windows
    [
    ("d",       withFocused (keysResizeWindow (0, 0) (0.5, 0.5))) -- Resize Smaller
    , ("s",       withFocused (keysResizeWindow (50,50) (1%2,1%2))) -- Resize Bigger
    , ("<Right>", withFocused (keysMoveWindow (40,0) )) -- Move Right
    , ("<Down>",  withFocused (keysMoveWindow (0,40) )) -- Move Down
--	, ("<Left>",  withFocused (keysMoveWindow (-40,0))) -- Move Left
--	, ("<Up>",    withFocused (keysMoveWindow (0,-40))) -- Move Up
    , ("S-s",     withFocused $ windows . W.sink) -- Sink

    , ("g",  moveFocusedWindowToRel (0,0)) -- Top Left
    , ("c",  moveFocusedWindowToRel (1%2, 0)) -- Top Center
    , ("r",  moveFocusedWindowToRel (1,0)) -- Top Right
    , ("h",  moveFocusedWindowToRel (0, 1%2)) -- Left Center
    , ("t",  moveFocusedWindowToRel (1%2, 1%2)) -- Center
    , ("n",  moveFocusedWindowToRel (1, 1%2)) -- Right Center
    , ("m",  moveFocusedWindowToRel (0,1)) -- Bottom Left
    , ("w",  moveFocusedWindowToRel (1%2, 1)) -- Bottom Center
    , ("v",  moveFocusedWindowToRel (1,1)) -- Bottom Right
    ] where
         moveFocusedWindowToRel (wMult, hMult) =
             do screenSize <- focusedScreenSize
                let screenX = round (wMult * fromIntegral (rect_width screenSize))
                    screenY = round (hMult * fromIntegral (rect_height screenSize))
                    sY = (if screenY == 0 then 40 else screenY)
                    sX = (if screenX == 0 then 40 else screenX)
                withFocused (keysMoveWindowTo (sX, sY) (wMult, hMult))

tagWindowKeymap = -- Window Tags
   [
   --   ("m",   withFocused (addTag "abc")) -- add tag "abc"
   -- , ("u",   withFocused (delTag "abc")) -- del tag "abc"
   -- , ("s",   withTaggedGlobalP "abc" W.sink) -- sink "abc"
   -- , ("d",   withTaggedP "abc" (W.shiftWin "2")) -- shift "abc" to 2
   -- , ("S-d", withTaggedGlobalP "abc" shiftHere) -- shift "abc" all here.
   -- , ("C-d", focusUpTaggedGlobal "abc") -- focus up all "abc"

     ("m",   tagPrompt myXPConfig (\s -> withFocused (addTag s))) -- add a tag to focused.
   , ("u",   tagPrompt myXPConfig (\s -> withFocused (delTag s))) -- delete a tag from focused.
   , ("S-u", tagDelPrompt myXPConfig) -- delete tag!
   , ("f",   tagPrompt myXPConfig (\s -> withTaggedGlobal s float)) -- float tagged
   -- , ("g",   tagPrompt myXPConfig (\s -> withTaggedP s (W.shiftWin "2"))) -- shift to 2
   , ("s",   tagPrompt myXPConfig (\s -> withTaggedGlobalP s W.sink)) -- sink
   , ("S-s", tagPrompt myXPConfig (\s -> withTaggedGlobalP s shiftHere)) -- shift here
   , ("'",   tagPrompt myXPConfig (\s -> focusUpTaggedGlobal s)) -- focus up.
   ]

-- BSP layout controls.
bspKeymap = -- BSP Layout Controls
    [ ("<Right>",   sendMessage $ BSP.ExpandTowards R) -- Expand Right
    , ("<Left>",    sendMessage $ BSP.ExpandTowards L) -- Expand Left
    , ("<Up>",      sendMessage $ BSP.ExpandTowards D) -- Expand Down
    , ("<Down>",    sendMessage $ BSP.ExpandTowards U) -- Expand Up
    , ("S-<Right>", sendMessage $ BSP.ShrinkFrom R) -- Shrink Right
    , ("S-<Left>",  sendMessage $ BSP.ShrinkFrom L) -- Shrink Left
    , ("S-<Down>",  sendMessage $ BSP.ShrinkFrom D) -- Shrink Down
    , ("S-<Up>",    sendMessage $ BSP.ShrinkFrom U) -- Shrink Up
    , ("r",         sendMessage BSP.Rotate) -- Rotate
    , ("s",         sendMessage BSP.Swap) -- Swap
    , ("p",         sendMessage BSP.FocusParent) -- Focus Parent
    , ("n",         sendMessage BSP.SelectNode) -- Select Node
    , ("m",         sendMessage BSP.MoveNode) -- Move Node
    , ("b",         sendMessage BSP.Balance) -- Balance
    , ("e",         sendMessage BSP.Equalize) -- Equalize
    ]

raiseKeymap = -- Raise
    [ ("v", runOrRaiseNext "Vivaldi" (className =? "Vivaldi")) -- Vivaldi
    , ("e", raiseNext (className =? "Emacs")) -- Emacs cycle
    , ("s", runOrRaise "Slack" (className =? "Slack")) -- Slack
    ]

    --    , ("M4-S-<Space>",  setLayout $ XMonad.layoutHook conf)
    -- , ("m", raiseMaybe     (runInTerm "-title mutt" "mutt") (title =? "mutt"))
    --    , ("M4-S-<Space>",  setLayout $ XMonad.layoutHook conf)

mainKeymap c = mkKeymap c $ -- Main Keys
  [ ("M4-S-<Return>",   spawn myTerminal) -- Terminal
    , ("M4-S-c",        kill) -- Kill window
    , ("M4-Insert",     pasteSelection) -- Paste selection
    , ("M4-<Space>",    sendMessage NextLayout) -- Next Layout
    , ("M4-S-<Space>",  myLayoutPrompt) -- Layout prompt
    , ("M4-k",          nextWindow)
    , ("M4-j",          prevWindow)
    , ("M4-S-k",        windows W.swapUp) -- Swap Up
    , ("M4-S-j",        windows W.swapDown) -- Swap Down
    , ("M4-<Tab>",      nextWindow) -- Next Window
    , ("M4-S-<Tab>",    prevWindow) -- Prev Window
    , ("M4-d", spawn "exe=`dmenu_run -fn myfontwsize -b -nb black -nf yellow -sf yellow` && eval \"exec $exe\"") -- dmenu
    , ("M4-t",          promptedGoto) -- Grid Select Workspace
    , ("M4-h",          goToSelected gsConfig2) -- Grid Select Window
    , ("M4-S-h",        bringSelected gsConfig2) -- Bring Grid Selected Window
    , ("M4-S-t",        promptedShift) -- Grid Select Shift
    , ("M4-S-a",        selectApps) -- Apps
    --, ("M4-C-a",        spawnSelected gsConfig ["krita","dolphin","repetierHost"]) -- Apps
    , ("M4-s",          sendMessage Shrink) -- Shrink
    , ("M4-z",          sendMessage Expand) -- Expand
    , ("M4-S-b",        sendMessage ToggleStruts) -- Toggle Struts
    , ("M4-q",          spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ("M4-S-q",        io $ exitWith ExitSuccess) -- Quit
    , ("M4-C-x",        spawn "xscreensaver-command -lock") -- screen lock
    , ("M4-x",          spawn "xscreensaver-command -activate")  -- screensaver
    , ("M4-v",          toSubmap c "tagWindowKeymap" tagWindowKeymap) -- tagged windows
    , ("M4-a",          toSubmap c "masterKeymap" masterKeymap) -- master pane
    , ("M4-b",          toSubmap c "bspKeymap" bspKeymap) -- BSP
    , ("M4-f",          toSubmap c "focusKeymap" focusKeymap) -- Focus
    , ("M4-u",          toSubmap c "floatKeymap" floatKeymap) -- Float
    , ("M4-l",          toSubmap c "layoutKeymap" layoutKeymap) -- Layout
    , ("M4-m",          toSubmap c "musicKeymap" musicKeymap) -- Music
    , ("M4-S-m",        toSubmap c "mainKeymap" []) -- Main
    , ("M4-p",          toSubmap c "promptsKeymap" promptsKeymap) -- Prompts
    , ("M4-r",          toSubmap c "raiseKeymap" raiseKeymap) -- Raise
    , ("M4-o",          toSubmap c "namedScratchpadsKeymap" namedScratchpadsKeymap) -- Scratchpad
    , ("M4-n",          toSubmap c "namedScratchpadsKeymap" namedScratchpadsKeymap) -- Scratchpad
    , ("M4-e",          getScratchpad) -- grid select scratchpad
    , ("M4-S-s",        toSubmap c "shotKeymap" shotKeymap) -- ScreenShot
    , ("M4-w",          toSubmap c "workspacesKeymap" workspacesKeymap) -- Workspaces
    , ("M4-/",          toSubmap c "promptSearchKeymap" promptSearchKeymap) -- Prompt Search
    -- dont really like this, going to to the prompt in the browser.
    -- , ("M4-S-/",        toSubmap c "selectSearchKeymap" selectSearchKeymap) -- Select Search
    , ("M4-i",          searchStuff) -- Search
    , ("M4-S-i",        selectSearchStuff) -- Search Selected
    , ("M4-c",          spawn "cellwriter@point") -- cellwriter at point.
    , ("M4-S-o",        spawn "onboard") -- onboard keyboard.
    ]
  where nextWindow      = windows W.focusDown
        prevWindow      = windows W.focusUp

------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--

fadeinactive = fadeInactiveLogHook fadeAmount
   where fadeAmount = 0.7

myConfig = do
  dbus <- D.connectSession
  getWellKnownName dbus
  return $ defaults {
      logHook = do
         ewmhDesktopsLogHook
         dynamicLogWithPP $ (prettyPrinter dbus)
         fadeinactive

      , manageHook = manageDocks <+> myManageHook <+> manageHook desktopConfig <+>
                     manageHook kdeConfig <+>
                     namedScratchpadManageHook scratchpads
      , layoutHook = layoutHook defaults
      , handleEventHook = docksEventHook <+> handleEventHook desktopConfig
      , startupHook = do
          docksStartupHook
          ewmhDesktopsStartup
          myStartupHook        -- >> setWMName "LG3D"
      }

-- docks :: XConfig myConfig -> XConfig myConfig

------------------------------------------------------------------------
  -- Combine it all together
-- A structure containing your confiuration settings, overriding
  -- fields in the default config. Any you don't override, will

-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--

  -- defaultConfig

defaults = kde4Config {
  -- simple stuff
   terminal           = myTerminal,
   focusFollowsMouse  = myFocusFollowsMouse,
   borderWidth        = myBorderWidth,
   modMask            = myModMask,
   workspaces         = myTopicNames,  -- MyWorkspaces
   normalBorderColor  = myNormalBorderColor,
   focusedBorderColor = myFocusedBorderColor,

     -- key bindings
   keys               = mainKeymap,
   mouseBindings      = myMouseBindings,

     -- hooks, layouts
   layoutHook         = myLayout, -- smartBorders $ myLayout,
   manageHook         = manageDocks <+> myManageHook,
     startupHook        = myStartupHook
} -- `additionalKeysP` myadditionalKeys

main :: IO ()
main = xmonad =<< myConfig


-- For KDE.
-- A kde example

-- import XMonad.Config.Kde

-- main = xmonad kdeConfig
--     { modMask = mod4Mask -- use the Windows button as mod
--     , manageHook = manageHook kdeConfig <+> myManageHook
--     }
