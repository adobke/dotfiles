--
-- File     : ~/.xmonad/xmonad.hs (for Xmonad >= 0.9)
-- Author   : Thayer Williams
-- Website  : http://cinderwick.ca/
-- Desc     : A simple, mouse-friendly xmonad config geared towards
--            netbooks and other low-resolution devices.
--
--            dzen is used for statusbar rendering, with optional mouse
--            integration provided by xdotool:
--
--             * left-click workspace num to go to that ws
--             * left-click layout to cycle next layout
--             * left-click window title to cycle next window
--             * middle-click window title to kill focused window

import XMonad 
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Actions.DwmPromote   -- swap master like dwm
import XMonad.Hooks.DynamicLog     -- statusbar 
import XMonad.Hooks.EwmhDesktops   -- fullscreenEventHook fixes chrome fullscreen
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.UrgencyHook    -- window alert bells 
import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import System.IO                   -- hPutStrLn scope

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.GridSelect

-- mine
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile



import qualified XMonad.StackSet as W   -- manageHook rules
main = do
        status <- spawnPipe myDzenStatus    -- xmonad status on the left
        conky  <- spawnPipe myDzenConky     -- conky stats on the right
        tray   <- spawn "stalonetray"
        xmonad $ withUrgencyHook dzenUrgencyHook $ defaultConfig 
            { modMask            = mod4Mask
            , terminal           = "urxvt"
            , borderWidth        = 2
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#0000ff"
            , handleEventHook    = fullscreenEventHook
            , workspaces = myWorkspaces
            , layoutHook = myLayoutHook
            , manageHook = manageDocks <+> myManageHook
                            <+> manageHook defaultConfig
            , logHook    = myLogHook status
            } 
            `additionalKeysP` myKeys

gsConfig = defaultGSConfig
    { gs_font = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
    , gs_cellheight = 30
    }

-- Tags/Workspaces
-- clickable workspaces via dzen/xdotool
myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ ["term","web","idle","music","extra"]
 
  where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]

-- Layouts
-- the default layout is fullscreen with smartborders applied to all
myLayoutHook = avoidStruts $ smartBorders ( tiled ||| spiral(6/7) ||| full ||| mtiled )
  where
    tiled   = named "T" $ ResizableTall 1 (3/100) (1/2) [] 
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled

    -- sets default tile as: Tall nmaster (delta) (golden ratio)

-- Window management
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "XCalc"          --> doFloat
    , className =? "Chromium"       --> doF (W.shift (myWorkspaces !! 1)) -- send to ws 2
    , className =? "Skype"       --> doF (W.shift (myWorkspaces !! 2)) -- send to ws 3
    , className =? "Gimp"           --> doF (W.shift (myWorkspaces !! 3)) -- send to ws 4
    , className =? "stalonetray"    --> doIgnore
    ]

-- Statusbar 
--
myLogHook h = dynamicLogWithPP $ myDzenPP
    {   ppOutput = hPutStrLn h
     ,  ppHiddenNoWindows = const ""
     , ppSep     = "^fg(#ffffff)^r(3x3)^fg() "
     , ppWsSep   = "^fg(#ffffff)|^fg()"
    }

myDzenStatus = "dzen2 -w '700' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -dock -x '0' -y '0' -expand left" ++ myDzenStyle
myDzenStyle  = " -h '14' -fg '#777777' -bg '#222222' -fn " ++ myFont
myFont = "-*-terminus-medium-r-normal-*-10-*-*-*-*-*-*-*"

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#8547c2" "" . wrap " " " "
    , ppHidden  = dzenColor "#dddddd" "" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#777777" "" . wrap " " " "
    , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
    , ppSep     = " ! "
    , ppLayout  = dzenColor "#a375d1" "" . wrap "^ca(1,xdotool key super+space)" " ^ca()"
    , ppTitle   = dzenColor "#ffffff" "" 
                    . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
                           "                          ^ca()^ca()" . shorten 40 . dzenEscape
    }

-- Key bindings
--
myKeys = [ ("M-b"        , sendMessage ToggleStruts              ) -- toggle the status bar gap
         , ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
         , ("M-S-<Return>" , dwmpromote                            ) -- swap the focused window and the master window
         , ("M-<Tab>"    , toggleWS                              ) -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                                ) -- go to next workspace
         , ("M-<Left>"   , prevWS                                ) -- go to prev workspace
         , ("M-S-<Right>", shiftToNext                           ) -- move client to next workspace
         , ("M-S-<Left>" , shiftToPrev                           ) -- move client to prev workspace
         , ("M-<Return>" , spawn $ "urxvt"                       )
         , ("M-c"        , spawn "xcalc"                         ) -- calc
         , ("M-p"        , spawn "gmrun"                         ) -- app launcher
         , ("M-n"        , spawn "wicd-client -n"                ) -- network manager
         , ("M-w"        , spawn "google-chrome"                      ) -- launch browser
         , ("M-S-w"      , spawn "google-chrome --incognito"          ) -- launch private browser
         , ("M-e"        , spawn "evince"                      ) -- launch file manager
         , ("C-M1-l"     , spawn "gnome-screensaver-command --lock"              ) -- lock screen
         , ("M-s"        , spawn "urxvtcd -e bash -c 'screen -dRR -S $HOSTNAME'" ) -- launch screen session
         , ("C-M1-<Delete>" , spawn "shutdown -r now"       ) -- reboot
         , ("C-M1-<Insert>" , spawn "shutdown -h now"       ) -- poweroff
         , ("M-r"        , shellPrompt defaultXPConfig      )
         , ("M-x"        , goToSelected gsConfig            )
         , ("M-<Up>"     , sendMessage MirrorShrink         )
         , ("M-<Down>"   , sendMessage MirrorExpand         )
         ]

-- vim:sw=4 sts=4 ts=4 tw=0 et ai 
