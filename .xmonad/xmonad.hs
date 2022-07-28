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
import XMonad.Layout.Dishes
import XMonad.Layout.LimitWindows
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane
import XMonad.Layout.DwmStyle
import XMonad.Layout.IM
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Dwindle as DW  -- Dwindle,Spiral and Squeeze.
import qualified XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.Tabbed
import XMonad.Layout.Roledex
import XMonad.Layout.Circle
import XMonad.Layout.Cross
import qualified XMonad.Layout.Grid as G
import XMonad.Layout.GridVariants ( Grid(..) )
import qualified XMonad.Layout.HintedTile as HT
import XMonad.Layout.StackTile
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
emacsn = "emacsn"

          ------------------------------------------------------------------------
-- Colors and borders
--
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#000000" -- "#ffb6b0"
myActiveBorderColor  = "#007c7c"
-- Colours
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

pbGreen     = "#08cc86"
pbDarkgreen = "#000a06"
pbRed       = "#fb4934"
pbDarkred   = "#cc241d"
pbYellow    = "#fabd2f"
pbBlue      = "#83a598"
pbPurple    = "#fab6fb"
pbCyan      = "#00FFFF"
pbAqua      = "#8ec07c"
pbWhite     = "#eeeeee"

pbBlue2     = "#2266d0"

-- orange = myColor "#9f6225"
-- pink   = myColor "#9f2562"
-- purple = myColor "#62259f"
myBack    = "#1a1a1a" -- Bar background
myFore    = "#999999" -- Bar foreground
myAcc     = "#25629f" -- Accent color
myHigh    = "#629f25" -- Highlight color
myLow     = "#000000" -- Lowlight color
myVis     = "#9f2562" -- Visible Workspace
myEmpt    = "#555555" -- Empty workspace

crizer :: String -> Bool -> X(String, String)
crizer _ False = return ("#002b36", "#839496")
crizer _ True = return ("#839596", "#002b36")

-- Colorizer generator
myColor color _ isFg = do
  return $ if isFg
           then (color, myLow)
           else (myLow ,color)

-- Colorizer colors for GridSelect
--aqua   = myColor "#259f62"
blue   = myColor "#25629f"
green  = myColor "#629f25"
red    = myColor "#964400"

-- basically tabs since I don't have anything else
myFont = "Source Code Pro"
myMonoFont = "Source Code Pro"
myfontwsize = "xft:" ++ myFont ++ ":size=16"

-----------------------------------------
-- Settings for the dzen sub-keymap popup

menuPopKeyColor = "yellow"
menuPopCmdColor = "cyan"
-- double double quoted so it can make it all the way to dzen.
menuPopLineHeight = "24"
menuPopDzenFont = "\"-*-ubuntu mono-*-*-*-*-*-96-*-*-*-*-*-*\""

-------------------------------------------------------------------------
--Xmonad prompt and GridSelect configs.

-- some nice colors for the prompt windows to match the dzen status bar.
myXPConfig = def --  defaultXPConfig                            -- (23)
    { fgColor = "#a8a3f7"
    , bgColor = "#3f3c6d"
    , font = "xft:Source Code Pro:size=14"
    , height = 96
    }

-- grid select workspaces
workspaceGsConfig = def -- defaultGSConfig
           {gs_colorizer = crizer
           , gs_cellheight  = 150
           , gs_cellpadding = 5
           , gs_cellwidth   = 400
           , gs_navigate   = navNSearch
           , gs_font = "xft:Source Code Pro:pixelsize=48"
           }

-- goto selected, bring to selected
gotoBringGsConfig = def
                {gs_cellheight = 75
                , gs_cellpadding = 5
                , gs_cellwidth = 600
                , gs_navigate   = navNSearch
                , gs_font = "xft:Source Code Pro:pixelsize=36"
                }


-- GridSelect config
-- get scratchpads, searchstuff, selectSearchStuff
-- give it a color for the grid when you use it.
coloredGSConfig colorizer = (buildDefaultGSConfig colorizer)
                       { gs_cellheight  = 95
                       , gs_cellpadding = 5
                       , gs_cellwidth   = 300
                       , gs_navigate   = navNSearch
                       , gs_font = "xft:Source Code Pro:pixelsize=40"
                       }

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

-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
               --
-- Workspaces using TopicSpace.
-- When a workspace is selected, if there are no windows present, open
-- whatever should be there, in the directory it should be focused on.
-- To reset a workspace, just make it empty, then change out and back.

data TopicItem = TI { topicName :: Topic
                    , topicDir  :: String
                    , topicAct  :: X ()
                    }

myTopics :: [TopicItem]
myTopics = [ TI "main" "" (return ())
           -- ,  TI "mail" "" (spawnInTopicDir "emacsn -e")
           , TI "Org" "org" (spawnInTopicDir "emacsn -m ORG")
           -- , TI "Web" "" (spawnInTopicDir "vivaldi-stable")
           , TI "Web" "" (spawnInTopicDir "emacsn -cws common -b duckduckgo.com" >>
                          spawnInTopicDir "discord")
--           , TI "Yeti" "play/Yeti/yeti-stack" (spawnInTopicDir "emacsn -m Yeti")
           , TI "Code" "play" (spawnInTopicDir "emacsn -m Code")
           , TI "QMK" "play/qmk_firmware/users/ericgebhart" (spawnInTopicDir "emacsn -m README.md")
           , TI "Elisp" "play/emacs-setup/elisp" (spawnInTopicDir "emacsn -m Elisp")
           , TI "Arch-Setup" "Arch-Setup" (spawnInTopicDir "emacsn -m Arch-Setup")
           , TI "XMonad" "play/xmonad-setup/.xmonad" (spawnInTopicDir "emacsn -m Xmonad xmonad.hs") -- lib/*/*.hs
           , TI "SPR" "play/Simple_Process_Repl" (spawnInTopicDir "emacsn -m SPR README.md")
           --, TI "RobotGirl" "play/robotgirl/" (spawnInTopicDir "emacsn -m RobotGirl")
           , TI "Plysp" "play/plysp" (spawnInTopicDir "emacsn -m Plysp")

           -- , TI "PBR" "play/Particle_Board_REPL" (spawnInTopicDir "emacsn -m PBR")

           , TI "eg.com" "play/ericgebhart.github.io" (spawnInTopicDir "emacsn -m eg.com")
           , TI "tb.com" "play/tangobreath.github.io" (spawnInTopicDir "emacsn -m tb.com")
           , TI "Closh" "play/closh" (spawnInTopicDir "emacsn -m closh")
           -- , TI "Mal" "play/mal" (spawnInTopicDir "emacsn -m mal")
           -- , TI "bgc-ui" "play/bgc-ui" (spawnInTopicDir "emacsn -m bgc-ui")
           -- , TI "emacs-single" "play/emacs-single" (spawnInTopicDir "emacsn -m emacs-single")
           , TI "Xournal" "Xournal" (spawnInTopicDir "xournal")
           , TI "Comm" "" (spawnInTopicDir "telegram-desktop" >>
                            spawnInTopicDir "emacsn -cews mail")

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
           , TI "Music"  "Music" (spawn "mediacenter26")
           , TI "Movies" "Movies" (spawn "vlc")
             -- , TI "calendar" "" (spawn "vivaldi --app='http://calendar.google.com'")
           , TI "Krita" "Drawings" (spawnInTopicDir "krita")
           , TI "Inkscape" "Drawings" (spawnInTopicDir "inkscape")
           , TI "Gravit" "Drawings" (spawnInTopicDir "GravitDesigner.AppImage")
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


-- --- Choose your method:  prompts or Grid select.  - These are prompts.
-- goto :: Topic -> X ()
-- goto = switchTopic myTopicConfig

-- promptedShift :: X ()
-- promptedShift = workspacePrompt myXPConfig $ windows . W.shift

--------------------------------------------------------------------------------
-- Polybar, xmobar, xfce4-panel, gnome-panel, kde-panel, etc.
--
-- pretty communication with the the dbus.
-- I use a completely transparent panel for this. The background image
-- has a nice multi-colored bar across the top of it. - oceanpark114.jpg
-- https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Hooks-DynamicLog.html

-- this one is for polybar

myPPPolybar :: D.Client -> PP
myPPPolybar dbus = def
    { ppOutput   = dbusPPOutput dbus
    , ppCurrent  = wrap ("%{F" ++ pbGreen ++ "}%{u" ++ pbGreen ++ "}%{+u}") " %{F- -u}"
    , ppVisible  = wrap ("%{F" ++ pbBlue ++ "} ") " %{F-}"
    , ppUrgent   = wrap ("%{F" ++ pbRed ++ "} ") " %{F-}"
    --, ppHidden = wrap " " " "
    , ppLayout   = wrap ("%{F" ++ pbCyan ++ "} ") " %{F-}"
    , ppHidden   = id . noScratchPad
    , ppWsSep    = "   "
    , ppSep      = "  |  "
    --, ppTitle = myAddSpaces 25
    , ppTitle           = shorten 80
    }
  where noScratchPad ws = if ws == "NSP" then "" else ws

-- this one for xfce4-panel or other panels that use pango/html type formatting
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

--- unused at the moment.
spawnToWorkspace :: String -> String -> X ()
spawnToWorkspace program workspace = do
  spawn program
  windows $ W.greedyView workspace

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()

dbusPPOutput :: D.Client -> String -> IO ()
dbusPPOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log")
                  (D.interfaceName_ "org.xmonad.Log")
                  (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant (UTF8.decodeString str)]
            }
    D.emit dbus signal

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


----------------------------------------------------------------------------
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
  [ NS "conky"   spawnConky findConky manageConky
  , NS "pavuControl"   spawnPavu findPavu managePavu
  , NS "term"  (myTerminal2 ++ " -t term") (title =? "term") (flexFloatBSP (1/20) (1/20))
  , NS "term1" (myTerminal2 ++ " -t term1") (title =? "term1") (flexFloatBSP (2/20) (2/20))
  , NS "term2" (myTerminal2 ++ " -t term2") (title =? "term2") (flexFloatBSP (3/20) (3/20))
  , NS "term3" (myTerminal2 ++ " -t term3") (title =? "term3") (flexFloatBSP (4/20) (4/20))
  , NS "term4" (myTerminal2 ++ " -t term4") (title =? "term4") (flexFloatBSP (6/20) (4/20))
  , NS "ghci"  (myTerminal2 ++ " -e ghci") (title =? "ghci") (flexFloatBSP (6/20) (1/10))
  --, NS "sync"  (myTerminal ++ " -e sy") (title =? "sy") (flexFloatSP (1/10) (2/3))
  , NS "top"   (myTerminal2 ++ " -e htop") (title =? "htop") (flexFloatSSP (1/4) (1/4))
 , NS "calc"  (myTerminal2 ++ " -e bcl -t bc") (title =? "bc") (flexFloatSSSP (1/4) (1/4))
  , NS "alsaMixer"  (myTerminal2 ++ " -e alsamixer -t alsamixer") (title =? "alsamixer") (flexFloatSSSP (1/4) (1/4))
  ]
  where
    spawnConky  = "conky -c ~/.config/conky/Erics.conkyrc" -- launch Conky
    findConky   = title =? "system_conky"   -- its window,  has a own_window_title of "system_conky"
    manageConky = (flexFloatSSP (1/4) (1/4))
    spawnPavu  = "pavucontrol"
    findPavu   = title =? "pavucontrol"
    managePavu = (flexFloatSSP (1/4) (1/4))

-- Scratchpad invocation / Dismissal
-- Warp
bringMouse = warpToWindow (9/10) (9/10)
scratchToggle a = namedScratchpadAction scratchpads a >> bringMouse

----------------------------------------------------------------
-- Grid-Select menu --------------------------------------------

-- This is how to make a runSelectedAction grid select menu.
-- A grid select for scratchpads.
myScratchpadMenu =
  [ ("Conky", (scratchToggle "conky"))
  , ("Volume",(scratchToggle "pavuControl"))
  , ("Music mixer", (scratchToggle "alsaMixer"))
  , ("GHCI",  (scratchToggle "ghci"))
  , ("Top",   (scratchToggle "top"))
  --, ("sync",  (scratchToggle "sync"))
  , ("Calc",  (scratchToggle "calc"))
  --, ("OSX",   (scratchToggle "OSX"))
  --, ("MSW",   (scratchToggle "MSW"))
  , ("Scratch", scratchpadSpawnActionTerminal  "urxvt")
  -- , ("Scratch", scratchpadSpawnActionTerminal  "urxvt -background rgba:0000/0000/0200/c800")
  -- at the end because that puts them on the outside edges of select-grid.
  , ("a Term", (scratchToggle "term"))
  , ("o Term", (scratchToggle "term1"))
  , ("e Term", (scratchToggle "term2"))
  , ("u Term", (scratchToggle "term3"))
  , ("h Term", (scratchToggle "term4"))
  ]

-- The scratch pad sub keymap
namedScratchpadsKeymap = -- Scratch Pads
    [("a", scratchToggle "term") -- Term
    , ("o", scratchToggle "term1") -- Term1
    , ("e", scratchToggle "term2") -- Term2
    , ("u", scratchToggle "term3") -- Term3
    , ("h", scratchToggle "term4") -- Term4
    , ("g", scratchToggle "ghci") -- ghci
    , ("c", scratchToggle "calc") -- calc
    , ("C", scratchToggle "conky") -- Conky
    , ("v", scratchToggle "pavuControl") -- Pavu Control
    , ("a", scratchToggle "alsaMixer") -- Pavu Control
    , ("t", scratchToggle "top") -- top
    , ("n", scratchpadSpawnActionTerminal "urxvt -background rgba:0000/0000/0200/c800") -- scratchpad
    ]

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.6
    w = 0.5
    t = 1 - h
    l = 1 - w

----------------------------------------------------------------
-- Grid-Select menu --------------------------------------------

-- More grid select Menus
-- I just never use these. Leaving in case I change my mind.
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

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- Pretty much I either float them, or ignore them. Shifting them, with
-- doShift, on invocation to another desktop is something I never do.
-- Topics automatically launches things by workspace, So I never have
-- a need for that.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
-- To match on the WM_NAME use 'title'
--
myManageHelpers = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- classFloats]
    , [ title       =? t --> doFloat           | t <- titleFloats]
    , [ resource    =? r --> doFloat           | r <- resourceFloats]
    , [ title       =? c --> doIgnore          | c <- titleIgnores]
    , [(className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
    ]
  where classFloats    = ["Galculator", "Steam", "Media_Center_26", "MPlayer", "Gimp", "Gajim.py", "Xmessage"]
        titleFloats    = ["Volume Control", "alsamixer", "Onboard"]
        resourceFloats = ["desktop_window", "Dialog", "gpicview"]
        titleIgnores   = ["stalonetray", "xfce4-notifyd"]

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

myManageHook = myManageHelpers <+>
    manageScratchPad

------------------------------------------------------------------------------------
--Layouts --------------------------------------------------------------------------

-- So Many layouts. Giving them names, keeping it down to ones that I use or used.
-- It's a bit easier to deal with individually Instead of with a big
-- intertwined where, mixed with the onworkspace directives it's too much.

myBSP             = BSP.emptyBSP
myCross           = simpleCross
myStackTile       = StackTile 1 (3/100) (1/2)
myHintedGrid      = G.GridRatio (4/3)
myDishes          = Dishes 2 (1/6)
myDwindle         = DW.Dwindle DW.R DW.CW (3/2) (11/10)
mySpiral          = DW.Spiral DW.L DW.CW (3/2) (11/10)
mySqueeze         = DW.Squeeze DW.D (3/2) (11/10)
myCols            = multiCol [1] 2 (3/100) (1/2)
my3ColMid         = ThreeColMid 1 (3/100) (1/2)
myBook            = ThreeColMid nmaster delta ratio
    where
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 2/3

myTwoPane         = TwoPane (3/100) (1/2)
myTall            = Tall 1 (3/100) (1/2)
myTabbed          = tabbed shrinkText tabConfig
myGrid            = Grid (8/4)
myNoBorders       = noBorders (fullscreenFull Full)
myAccordion       = Accordion
myMAccordion      = Mirror (Accordion)
myWide2           = Mirror (Tall 1 (3/100) (1/2))

myWide            = Mirror $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 80/100

-- Magnifier layouts.
-- Increase the size of the window that has focus by a custom zoom, unless if it is the master window.
-- myCode            = limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10

myMagBSP          = Mag.magnifier BSP.emptyBSP

mySplit = Mag.magnifiercz' 1.4 $ Tall nmaster delta ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Percent of screen to increment by when resizing panes
        delta   = 3/100
        -- Default proportion of screen occupied by master pane
        ratio   = 60/100

myMagTiled = Mag.magnifier tiled
  where
    tiled = Tall tnmaster tdelta ratio
    tnmaster = 1
    ratio = 1/2
    tdelta = 3/100

myHTTall = hintedTile HT.Tall
  where
     hintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft
     nmaster    = 1
     ratio      = 1/2
     delta      = 3/100

myHTWide = hintedTile HT.Wide
  where
     hintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft
     nmaster    = 1
     ratio      = 1/2
     delta      = 3/100

-- with IM to get a sidebar with grid in the other part
myChat' l = withIM size roster l
    where
        -- Ratio of screen roster will occupy
        size = 1%5
        -- Match roster window
        roster = Title "Buddy List"
myChat = myChat' G.Grid

-- For tailing logs ?
myDish = limitWindows 5 $ Dishes nmaster ratio
    where
        -- The default number of windows in the master pane
        nmaster = 1
        -- Default proportion of screen occupied by other panes
        ratio = 1/5

-- example of withIM
-- Dishes with a sidebar for ProcMeter3 or whatever
myDishBar' l = reflectHoriz $ withIM procmeterSize procmeter $
              reflectHoriz $ l
    where
        -- Ratio of screen procmeter will occupy
        procmeterSize = 1%7
        -- Match procmeter
        procmeter = ClassName "ProcMeter3"
myDishesSidebar = myDishBar' myDish

-----------------------------------------------------------------------
-- Group the layouts and assign them to workspaces.

genLayouts = avoidStruts (
    named "BSP" BSP.emptyBSP |||
    named "GGrid" G.Grid |||
    named "GridH" myGrid |||
    named "3ColMid" my3ColMid |||
    named "Columns" myCols |||
    named "Tall" myTall |||
    named "Wide" myWide |||
    named "Wide2" myWide2 |||
    named "Dishes" myDish |||
    named "HintTall" myHTTall |||
    named "HintWide" myHTWide |||
    named "Tabbed" myTabbed |||
    named "Roledex" Roledex |||
    named "TwoPane" myTwoPane |||
    named "Circle" Circle |||
    named "Cross" myCross |||
    named "Book" myBook |||
    named "Mag BSP" myMagBSP |||
    named "Mag Tiled" myMagTiled |||
    named "Stack Tile" myStackTile |||
    named "Accordion" myAccordion |||
    named "Accordion V" myMAccordion |||
    named "Spiral" mySpiral |||
    named "Squeeze" mySqueeze |||
    named "Dwindle" myDwindle |||
    named "Full" Full |||
    named "NoBorders" myNoBorders)

---------------------------------------------------------
-- A menu/grid-select menu for the named layouts above.
-- All the names. For the prompt and the GridSelect.
allLayouts = ["BSP"
             , "GGrid"
             , "GridH"
             , "3ColMid"
             , "Columns"
             , "Tall"
             , "Wide"
             , "Wide2"
             , "Dishes"
             , "HintTall"
             , "HintWide"
             , "Tabbed"
             , "Roledex"
             , "TwoPane"
             , "Circle"
             , "Cross"
             , "Book"
             , "Mag BSP"
             , "Mag Tiled"
             , "Stack Tile"
             , "Accordion"
             , "Accordion V"
             , "Spiral"
             , "Squeeze"
             , "Dwindle"
             , "Full"
             , "NoBorders"
             ]

-- The gridSelect menu.
layoutsGS = [ (s, sendMessage $ JumpToLayout s) | s <- allLayouts ]

myLayout = avoidStruts $
  onWorkspace "Web" simpleSquares $
  onWorkspaces ["Code", "QMK", "Xmonad", "Elisp", "Arch Setup"] codeLayouts $

  onWorkspace "kicad" kicadLayouts $
  onWorkspace "comm" commLayouts $

  onWorkspaces ["krita", "inkscape", "gravit", "Xournal"] artLayouts $

  onWorkspace "3D" threeDLayouts $
  onWorkspace "BD" bdLayouts $

  onWorkspace "Music" mediaLayouts $
  onWorkspace "Movies" mediaLayouts $
  onWorkspace "French" langLayouts $
  onWorkspace "Downloads" bdLayouts $

  genLayouts
    where
        simpleSquares = myTwoPane ||| myMagTiled   ||| myTall       ||| myCols       ||| myGrid      ||| myMagTiled
        kicadLayouts  = myMagBSP  ||| myMagTiled   ||| myTall       ||| myCross      ||| Circle
        codeLayouts   = myMAccordion ||| myTall    ||| myGrid       ||| Roledex
        commLayouts   = myMagBSP  ||| myMagTiled   ||| myTall       ||| myBSP        ||| myStackTile ||| myGrid
        artLayouts    = Full      ||| myMagBSP     ||| myMAccordion ||| myTall       ||| myDwindle   ||| Circle
        threeDLayouts = myMagBSP  ||| myMagTiled   ||| myMAccordion ||| myDwindle    ||| mySpiral
        bdLayouts     = mySplit   ||| myMAccordion ||| myMagTiled   ||| myStackTile
        mediaLayouts  = Full      ||| myNoBorders  ||| myGrid       ||| Circle       ||| myCross     ||| myTall
        langLayouts   = mySqueeze ||| myMagBSP     ||| myMagTiled   ||| myMAccordion ||| myDwindle   ||| mySpiral

-- layout prompt (w/ auto-completion and all layouts)
myLayoutPrompt = inputPromptWithCompl mylayoutXPConfig "Layout"
                    (mkComplFunFromList' allLayouts)
                    ?+ (sendMessage . JumpToLayout)

mylayoutXPConfig = def { autoComplete = Just 1000 }

-- modified variant of cycleRecentWS from XMonad.Actions.CycleRecentWS (17)
-- which does not include visible but non-focused workspaces in the cycle
cycleRecentWS' = cycleWindowSets options
 where options w = map (W.view `flip` w) (recentTags w)
       recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]


------------------------------------------------------------------------------------------
-- Prompts and searches and stuff.   GridSelect...

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
synonymes  = searchEngine "synonyms"   "http://www.synonymes.com/synonyme.php?mot="
-- synonymes = searchEngine  "synonymes" "http://www.les-synonymes.com/mot/"
wiktionaryen = searchEngine "wiktionary" "https://en.wiktionary.org/w/index.php?search="
wiktionnaire = searchEngine "wiktionnaire" "https://fr.wiktionary.org/w/index.php?search="
clojuredocs = searchEngine "clojuredocs" "https://clojuredocs.org/clojure.core/"

----------------------------------------------------------------
-- Grid-Select menus -------------------------------------------

promptSearchMenu =
     [("duckduckgo",   (mypromptSearch duckduckgo))
     , ("Reverso",      (mypromptSearch reverso))
     , ("Wiktionary",   (mypromptSearch wiktionaryen))
     , ("Wiktionnaire", (mypromptSearch wiktionnaire))
     , ("Synonymes.fr", (mypromptSearch synonymes))
     , ("man",          (manPrompt myXPConfig))
--     , ("sdcv",         (sdcvPrompt myXPConfig "sdcv"))
--     , ("calc",         (calcPrompt myXPConfig "calc"))
     , ("goldendict",   (gDPrompt myXPConfig))
     , ("google",       (mypromptSearch google))
     , ("hoogle",       (mypromptSearch hoogle))
     , ("clojuredocs",  (mypromptSearch clojuredocs))
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
     , ("Arch",         (mypromptSearch arch))
     , ("Arch Pkg",     (mypromptSearch archpkgs))
     , ("Arch AUR",     (mypromptSearch archaur))
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
     , ("Wiktionary",   (selectSearch wiktionaryen))
     , ("Wiktionnaire", (selectSearch wiktionnaire))
     , ("Synonymes.fr", (selectSearch synonymes))
     ]

-------------------------------------------------------------------------------------------
-- Grid-select
-- The actual grid-select menu functions.
-- Automatic workspace grid select by getting the names from the topicspace.

-- Default navigation.  arrows/vi (hjkl) '/' for string search.

wsgrid = withWindowSet $ \w -> do
    let wss = W.workspaces w
        usednames = map W.tag $  wss
        newnames = filter (\used -> (show used `notElem` (map show myTopicNames))) usednames
    gridselect workspaceGsConfig (map (\x -> (x,x)) (myTopicNames ++ newnames))

-- ppSort = fmap (.scratchpadFilterOutWorkspace) getSortByTag
-- where noScratchPad ws = if ws == "NSP" then "" else ws

-- getSortByIndexNoNSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
-- getSortByIndexNoNSP =
--   fmap ((. namedScratchpadFilterOutWorkspace) . (. filter (\(W.Workspace tag _ _) -> tag /= "0"))) getSortByIndex
-- getSortByIndexNoNSP =
--   fmap (. filter (\(W.Workspace tag _ _) -> tag /= "0" && tag /= "NSP" && tag /= "NSP1")) getSortByIndex

-- myExtraWSs =  ["0","NSP","NSP1"]
-- myWorkspaces = map show [1..9] ++ myExtraWSs
-- getSortByIndexNoNSP = fmap (. filter (\(W.Workspace tag _ _) -> not (tag `elem` myExtraWSs))) getSortByIndex


-- gridselect a workspace and view it
promptedGoto = wsgrid >>= flip whenJust (switchTopic myTopicConfig)

-- gridselect a workspace to shift active window to
promptedShift = wsgrid >>= flip whenJust (windows . W.shift)

warpToCentre = gets (W.screen . W.current . windowset) >>= \x -> warpToScreen x  0.5 0.5

selectApps   = runSelectedAction (coloredGSConfig green) myApps
getScratchpad = runSelectedAction (coloredGSConfig blue) myScratchpadMenu
searchStuff = runSelectedAction (coloredGSConfig green) promptSearchMenu
selectSearchStuff = runSelectedAction (coloredGSConfig green) selectSearchMenu
layoutGridSelect = runSelectedAction (coloredGSConfig red) layoutsGS
---------------------------------------------------------------------------
--- Key Map doc dzen popup ------------------------------------------------
---------------------------------------------------------------------------
-- Basically, this uses an awk script to parse xmonad.hs for the keymaps.
-- When you hit a key combo that has a subkeymap this script parses xmonad.hs
-- and displays a dzen popup with the possible completion choices for that
-- key submap.  so M-o gives me a popup of the possible scrathpads and their
-- corresponding key...  There might be a better way. But this works fine.

windowScreenSize :: Window -> X (Rectangle)
windowScreenSize w = withDisplay $ \d -> do
    ws <- gets windowset
    sc <- fromMaybe (W.current ws) <$> pointScreen 10 10 -- who cares where.

    return $ screenRect . W.screenDetail $ sc
  where fi x = fromIntegral x

focusedScreenSize :: X (Rectangle)
focusedScreenSize =
  withWindowSet $ windowScreenSize . fromJust . W.peek


keyMapDoc :: String -> X Handle
keyMapDoc name = do
       ss <- focusedScreenSize
       handle <- spawnPipe $ unwords ["~/.xmonad/showHintForKeymap.sh",
                                       name,
                                       show (rect_x ss),
                                       show (rect_y ss),
                                       show (rect_width ss),
                                       show (rect_height ss),
                                       menuPopKeyColor,
                                       menuPopCmdColor,
                                       menuPopDzenFont,
                                       menuPopLineHeight]
       return handle

-- send it off to awk. -> showHintForKeymap
toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  pipe <- keyMapDoc name
  submap $ mkKeymap c m
  io $ hClose pipe

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Key bindings  EZ, lots of submaps.  Scratchpads are up above with
-- their friends.
--
-- Note: Formatting is important for script.  The comment becomes the menu text.
focusKeymap = -- Focus
  [ ("v",       focus "vivaldi") -- Focus Vivaldi
  , ("e",       focus "emacs") -- Focuse Emacs
  , ("m",       windows W.focusMaster) -- Focus Master
  , ("s",       windows W.swapMaster) -- Swap Master
  , ("/",       spawn menu) -- Menu
  , ("t",       withFocused $ windows . W.sink) -- Sink
  , ("<Up>",    windows W.swapUp) -- Swap Up
  , ("<Down>",  windows W.swapDown) -- Swap Down
  , ("z",       rotOpposite) -- Rotate Opposite
  , ("i",       rotUnfocusedUp) -- Rotate UnFocused UP
  , ("d",       rotUnfocusedDown) -- Rotate Focused Down
  , ("r",       refresh) -- Refresh
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


-- M4-S-m to get a menu of this in the dzen popup.  Comment is menu entry if present code otherwise.
mainKeymap c = mkKeymap c $ -- Main Keys
    [ ("M4-S-m",          toSubmap c "mainKeymap" []) -- Main (This) Keymap
    , ("M4-<Return>",     spawn myTerminal) -- Terminal
    , ("M4-S-c",        kill) -- Kill window
    , ("M4-Insert",     pasteSelection) -- Paste selection
    , ("M4-<Space>",    sendMessage NextLayout) -- Next Layout
--    , ("M4-S-l",        myLayoutPrompt) -- Layout prompt
    , ("M4-<R>",          nextWindow)
    , ("M4-<L>",          prevWindow)
    , ("M4-<U>",        windows W.swapUp) -- Swap Up
    , ("M4-<D>",        windows W.swapDown) -- Swap Down
--    , ("M4-<Tab>",      nextWindow) -- Next Window
--    , ("M4-S-<Tab>",    prevWindow) -- Prev Window
    , ("M4-d", spawn "exe=`dmenu_run -fn myfontwsize -b -nb black -nf yellow -sf yellow` && eval \"exec $exe\"") -- dmenu
    , ("M4-S-d",        spawn "rofi -show drun") -- Rofi

-- Grid Select
    , ("M4-t",          promptedGoto) -- Workspaces -GS
    , ("M4-l",          layoutGridSelect) -- Layouts -GS
    , ("M4-h",          goToSelected gotoBringGsConfig) -- Windows -GS
    , ("M4-S-h",        bringSelected gotoBringGsConfig) -- Bring Window to Selected  -GS
    , ("M4-S-t",        promptedShift) -- Shift Window to Selected -GS
    , ("M4-S-a",        selectApps) -- Apps -GS
    , ("M4-i",          searchStuff) -- Search
    , ("M4-S-i",        selectSearchStuff) -- Search Selected
--    , ("M4-e",          getScratchpad) -- grid select scratchpad
    --, ("M4-C-a",        spawnSelected workspaceG sConfig ["krita","dolphin","repetierHost"]) -- Apps

    , ("M4-<Tab>",      cycleRecentWS' [xK_Super_L, xK_Shift_L] xK_Tab xK_grave) -- Cycle Recent

-- prompted things
    , ("M4-p",          toSubmap c "promptsKeymap" promptsKeymap) -- Prompts
    -- tend to use "i" instead
    , ("M4-C-m",        manPrompt myXPConfig) -- Man Pages

 -- Scratchpads
    , ("M4-e",          toSubmap c "namedScratchpadsKeymap" namedScratchpadsKeymap) -- Scratchpad
--  Or on the home row with M4-Control
    , ("M4-M1-a", scratchToggle "term") -- Term
    , ("M4-M1-o", scratchToggle "term1") -- Term1
    , ("M4-M1-e", scratchToggle "term2") -- Term2
    , ("M4-M1-u", scratchToggle "term3") -- Term3
    , ("M4-M1-h", scratchToggle "term4") -- Term4
    , ("M4-M1-g", scratchToggle "ghci") -- ghci
    , ("M4-M1-c", scratchToggle "calc") -- calc
    , ("M4-M1-t", scratchToggle "top") -- top
    , ("M4-M1-s", scratchToggle "conky") -- Conky
    , ("M4-M1-v", scratchToggle "pavuControl") -- Pavu Control
    , ("M4-M1-S-v", scratchToggle "alsaMixer") -- Pavu Control
    , ("M4-M1-n", scratchpadSpawnActionTerminal "urxvt -background rgba:0000/0000/0200/c800") -- scratchpad

    -- Sink
    , ("M4-r",   withFocused $ windows . W.sink) -- sink focused window
    , ("M4-S-r", sinkAll) -- sink all windows

    , ("M4-s",          sendMessage Shrink) -- Shrink
    , ("M4-z",          sendMessage Expand) -- Expand
    , ("M4-S-b",        sendMessage ToggleStruts) -- Toggle Struts

 -- recompile/reload - Quit
    , ("M4-q",          spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ("M4-S-q",        io $ exitWith ExitSuccess) -- Quit

-- Screensaver
    , ("M4-S-x",        spawn "xscreensaver-command -lock") -- screen lock
    , ("M4-x",          spawn "xscreensaver-command -activate")  -- screensaver

    -- Mark windows and manipluate with tags.
    , ("M4-m",          toSubmap c "tagWindowKeymap" tagWindowKeymap) -- tagged windows


-- window and workspace manipulations
-- bsp layout manipulations
    , ("M4-b",          toSubmap c "bspKeymap" bspKeymap) -- BSP
    , ("M4-a",          toSubmap c "masterKeymap" masterKeymap) -- master pane
    , ("M4-f",          toSubmap c "focusKeymap" focusKeymap) -- Focus
    , ("M4-u",          toSubmap c "floatKeymap" floatKeymap) -- Float
    , ("M4-w",          toSubmap c "workspacesKeymap" workspacesKeymap) -- Workspaces
    --, ("M4-l",          toSubmap c "layoutKeymap" layoutKeymap) -- Layout
    , ("M4-m",          toSubmap c "musicKeymap" musicKeymap) -- Music
    , ("M4-r",          toSubmap c "raiseKeymap" raiseKeymap) -- Raise

-- screenshots
    , ("M4-S-s",        toSubmap c "shotKeymap" shotKeymap) -- ScreenShot

-- dzen prompt search
--    , ("M4-/",          toSubmap c "promptSearchKeymap" promptSearchKeymap) -- Prompt Search
    -- dont really like this, going to to the prompt in the browser.
    -- , ("M4-S-/",        toSubmap c "selectSearchKeymap" selectSearchKeymap) -- Select Search

-- keyboardless needs
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

--     , manageHook = manageHook kdeConfig <+> myManageHook

myConfig = do
  dbus <- D.connectSession
  getWellKnownName dbus
  return $ defaults {
      logHook = do
         ewmhDesktopsLogHook
         -- dynamicLogWithPP $ (prettyPrinter dbus)
         dynamicLogWithPP $ (myPPPolybar dbus)
         fadeinactive

      , manageHook = manageDocks <+> myManageHook <+> manageHook desktopConfig <+>
-- for kde
--                   manageHook kdeConfig <+>
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

defaults = def {
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
--     {
--     , manageHook = manageHook kdeConfig <+> myManageHook
--     }
