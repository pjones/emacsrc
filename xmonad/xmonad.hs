-- Load XMonad
import XMonad
import qualified XMonad.StackSet as W

-- XMonad contrib (Actions)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer

-- XMonad contrib (Hooks)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

-- XMonad contrib (Layouts)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile

-- Utilities
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad.Util.Paste
import XMonad.Util.Run

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask = mod3Mask
    , terminal = "urxvtc"
    , borderWidth = 2
    , normalBorderColor = "#1a1a1a"
    , focusedBorderColor = "#00bfff"
    , workspaces = myWorkspaces
    , keys = myKeys
    , logHook = fadeInactiveLogHook 0.8 
                >> (updatePointer (Relative 1 1))
                >> (dynamicLogWithPP $ myPP xmproc)
    , layoutHook = myLayoutRules
    , manageHook = manageHook defaultConfig <+> manageDocks
    }

myWorkspaces = 
  [ "1:Dev"
  , "2:Web"
  , "3:Comm"
  , "4:VNC"
  , "5:Fire"
  , "6"
  , "7"
  , "8"
  , "9"
  ]

myPP output = defaultPP
  { ppCurrent = xmobarColor "#7b79b1" "#0f141f" . wrap "[" "]"
  , ppVisible = wrap "(" ")"
  , ppHiddenNoWindows = const ""
  , ppSep = " » "
  , ppTitle = xmobarColor "#7b79b1" "" . shorten 40
  , ppUrgent = xmobarColor "#f92672" "#0f141f"
  , ppWsSep = " "
  , ppExtras = []
  , ppOutput = hPutStrLn output
  }

myDefaultLayout = (tall ||| full)
  where
    tall = ResizableTall 1 (3/100) (1/2) []
    full = noBorders Full

myLayoutRules = avoidStruts $
                onWorkspace (myWorkspaces !! 2) (skinny ||| tall) myDefaultLayout
              where
                tall   = ResizableTall 1 (3/100) (1/2)  []
                skinny = ResizableTall 1 (3/100) (9/10) []

-- Keys I use to jump workspaces without a modifier.    
myWorkspaceKeys = 
  [ 0x1008ff03 -- keycode 232: F1 without the Fn key pressed
  , 0x1008ff02 -- keycode 233: F2 "       "   "  "   "
--, ?????????? -- keycode 128: F3 "       "   "  "   "
--, ?????????? -- keycode 212: F4 "       "   "  "   "
  , 0x1008ff06 -- keycode 237: F5 "       "   "  "   "
  , 0x1008ff05 -- keycode 238: F6 "       "   "  "   "
  ]

-- Other keys on this keyboard
-- keycode 191 keysym 0x0: F13
-- keycode 192 keysym 0x0: F14
-- keycode 193 keysym 0x0: F15
-- keycode 194 keysym 0x0: F16
-- keycode 195 keysym 0x0: F17
-- keycode 196 keysym 0x0: F18
-- keycode 197 keysym 0x0: F19
-- keycode 169 keysym 0x1008ff2c: XF86Eject

-- Use C-z as a prefix key, and have all other keys come under it.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((controlMask, xK_z), submap . M.fromList $
      [ ((controlMask, xK_z), toggleWS)
      , ((0,           xK_z), sendKey controlMask xK_z)

      -- Focusing and swapping windows
      , ((0,         xK_n),  windows W.focusDown)
      , ((0,         xK_p),  windows W.focusUp)
      , ((0,         xK_o),  windows W.focusDown)
      , ((shiftMask, xK_n),  windows W.swapDown)
      , ((shiftMask, xK_p),  windows W.swapUp)
      , ((0,         xK_m),  windows W.focusMaster)
      , ((shiftMask, xK_m),  promote)
      , ((shiftMask, xK_t),  withFocused $ windows . W.sink)
      , ((0,         xK_c),  kill)

      -- Control Xmonad (restart, quit)
      , ((shiftMask, xK_q),  io (exitWith ExitSuccess))
      , ((0,         xK_q),  spawn "xmonad --recompile && xmonad --restart")
      , ((0,         xK_b),  sendMessage ToggleStruts)

      -- Controlling X (sending keys, pasting, etc)
      , ((0,         xK_y), pasteSelection)

      -- Spawning other applications
      , ((0,         xK_space), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
      , ((0,         xK_t),     spawn $ XMonad.terminal conf)
      ]
      ++

      -- Switch workspaces and move windows to other workspaces
      [((m, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
      ++
      -- Switch screens and move workspaces to other screens
      [((m, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]])

    -- Remaining keys that use the XMonad modifier key

    -- Window resizing
    , ((modm, xK_Left),  sendMessage Shrink)
    , ((modm, xK_Right), sendMessage Expand)
    , ((modm, xK_Up),    sendMessage MirrorShrink)
    , ((modm, xK_Down),  sendMessage MirrorExpand)

    -- Switching layouts
    , ((modm,               xK_space), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

    -- Controlling XMMS2
    , ((0, xF86XK_AudioPlay),        spawn "xmms2 toggle")
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioMute),        spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioPrev),        spawn "xmms2 prev")
    , ((0, xF86XK_AudioNext),        spawn "xmms2 next")
  ]
