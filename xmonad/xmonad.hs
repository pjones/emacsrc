import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Util.Paste
import XMonad.Util.Run
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import Graphics.X11.ExtraTypes.XF86
import System.Exit

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask = mod3Mask
    , terminal = "urxvt"
    , borderWidth = 2
    , normalBorderColor = "#1a1a1a"
    , focusedBorderColor = "#00bfff"
    , keys = myKeys
    , logHook = fadeInactiveLogHook 0.8 >> (dynamicLogWithPP $ myPP xmproc)
    , layoutHook = myLayoutRules
    , manageHook = manageHook defaultConfig <+> manageDocks
    }

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

myLayoutRules = avoidStruts $
  (tall ||| full)
  where
    tall = Tall 1 (3/100) (1/2)
    full = noBorders Full

-- Use C-z as a prefix key, and have all other keys come under it.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((controlMask, xK_z), submap . M.fromList $
      -- Send C-z (when pressed twice)
      [ ((controlMask, xK_z), sendKey controlMask xK_z)

      -- Focusing and swapping windows
      , ((0,         xK_n),  windows W.focusDown)
      , ((0,         xK_p),  windows W.focusUp)
      , ((0,         xK_o),  windows W.focusDown)
      , ((shiftMask, xK_n),  windows W.swapDown)
      , ((shiftMask, xK_p),  windows W.swapUp)
      , ((0,         xK_m),  windows W.focusMaster)
      , ((shiftMask, xK_m),  windows W.swapMaster)
      , ((shiftMask, xK_t),  withFocused $ windows . W.sink)
      , ((0,         xK_c),  kill)

      -- Switch back to the previous workspace
      -- , ((0,         ?),  toggleWS)

      -- Control Xmonad (restart, quit)
      , ((shiftMask, xK_q),  io (exitWith ExitSuccess))
      , ((0,         xK_q),  spawn "xmonad --recompile; xmonad --restart")
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

    -- Resizing the master window
    , ((modm,      xK_comma),  sendMessage Shrink)
    , ((modm,      xK_period), sendMessage Expand)

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
