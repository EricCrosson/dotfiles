
import XMonad
import System.IO
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FloatNext

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import qualified XMonad.Actions.Search as S
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowBringer
import XMonad.Actions.WindowGo
import XMonad.Actions.Volume
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicWorkspaceGroups

import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders

import XMonad.Prompt

myTerminal = "xterm"
myBrowser = "uzbl-tabbed"

myHighlight = "#00FF00"
myLowlight = "#00008A"
myEmpty = "#999999"
myVisible = "#FFFF00"
myForeground = "#FFFFFF"
myBackground = "#000000"
myAccent = "#85E0FF"

myBar = "xmobar /home/dallas/.xmobarrc"
myBorderWidth = 2
myWorkspaces = ["1"
               ,"2"
               ,"3"
               ,"4"
               ,"5"
               ]

main = do
    h <- spawnPipe myBar -- used in logHook
    xmonad $ defaultConfig
        { terminal = myTerminal
        , workspaces = myWorkspaces
        , focusedBorderColor = myAccent
        , borderWidth = myBorderWidth
        , manageHook =  myManageHook
        , layoutHook = avoidStruts $
                       smartBorders $
                       spacing 2 $
                       layoutHook defaultConfig
        , startupHook = myStartup
        -- , logHook = myLogHook h
        , modMask = mod4Mask
        }
        `additionalKeysP` addedKeys

myScratchpads = [ NS "alsamixer" "xterm -e alsamixer" (title =? "alsamixer") (customFloating $ W.RationalRect 0.6 0.1 0.35 0.5)
                , NS "xterm" "xterm -name scratchterm" (title =? "scratchterm") (customFloating $ W.RationalRect 0.05 0.8 0.9 0.15)
                ]

myStartup = do
    spawn "emacs --daemon"
    spawn "xmodmap ~/.xmodmap"
    spawn "xscreensaver -no-splash &"
    spawn "xterm"

addedKeys = [("M4-r", spawn "dmenu_run")
            ,("M4-=", refresh) -- why doesn't this work?
            ,("M4-n", nextWS)
            ,("M4-p", prevWS)
            ,("M4-<Tab>", toggleWS)
            ,("M4-s M4-l", spawn "xscreensaver-command --lock")
            ,("M4-s M4-s", spawn "scrot ~/Documents/screenshots/%Y-%m-%d-%T-screenshot.png")
            ,("M4-w g", gotoMenu)
            ,("M4-w b", bringMenu)
            ,("M4-<F1>", runOrRaise "emacsclient -c -a emacs " (className =? "Emacs"))
            ,("M4-<F2>", runOrRaise "uzbl-tabbed" (className =? "Uzbl-tabbed"))
            ,("M4-<F12>", namedScratchpadAction myScratchpads "alsamixer")
            ,("M4-S-<F12>", spawn "amixer -D pulse set Master toggle")
            ,("M4-x M4-k", spawn "xkill")
            ,("M4-<Return>", namedScratchpadAction myScratchpads "xterm")
            ,("M4-S-<Return>", spawn "xterm -e /home/dallas/scripts/screen.sh")
            ]
            -- Search functionality (thanks tylevad on Github!)
            ++ [("M4-s " ++ k, S.promptSearchBrowser myXPConfig myBrowser f) | (k,f) <- searchEngines]
               where searchEngines = [ ("g", S.google)
                                     , ("d", S.searchEngine "DuckDuckGo" "https://duckduckgo.com/?q=")
                                     , ("w", S.searchEngine "Wikipedia" "http://en.wikipedia.org/wiki/Special:Search?search=")
                                     , ("y", S.searchEngine "YouTube" "https://www.youtube.com/results?search_query=")
                                     , ("a", S.searchEngine "ArchWiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
                                     ]

myManageHook = composeAll [ manageDocks
                          , namedScratchpadManageHook myScratchpads
                          ]

-- myLogHook h = (dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ myPP h)

myPP h = xmobarPP
  { ppCurrent         = xmobarColor myEmpty "" . wrap "{" "}" . xmobarColor myHighlight ""
  , ppVisible         = xmobarColor myEmpty "" . wrap "[" "]" . xmobarColor myVisible ""
  , ppHidden          = xmobarColor myForeground ""
  , ppHiddenNoWindows = xmobarColor myEmpty ""
  , ppTitle           = xmobarColor myForeground "" . shorten 100
  , ppLayout          = xmobarColor myAccent ""
  , ppSep             = " <fc=" ++ myLowlight ++ ">|</fc> "
  , ppWsSep           = " "
  , ppOutput          = hPutStrLn h
  }

myXPConfig = defaultXPConfig
  { fgColor = myForeground
  , bgColor = myBackground
  , bgHLight = myBackground
  , fgHLight = myAccent
  , borderColor = myAccent
  , position = Bottom
  , historySize = 0
  , height = 16
  }
