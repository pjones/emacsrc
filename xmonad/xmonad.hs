-- Load XMonad
import XMonad
import qualified XMonad.StackSet as W

-- XMonad contrib (Actions)
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.OnScreen (viewOnScreen, greedyViewOnScreen)

-- XMonad contrib (Hooks)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops

-- XMonad contrib (Layouts)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named (named)

-- Utilities
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import Control.Monad (unless)
import XMonad.Util.Types
import XMonad.Util.Run
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Scratchpad

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask = mod3Mask
    , terminal = "urxvtc"
    , borderWidth = 3
    , normalBorderColor = "#1a1a1a"
    , focusedBorderColor = "#00bfff"
    , workspaces = myWorkspaces
    , keys = myKeys
    , logHook = fadeInactiveLogHook 0.8 
                >> (updatePointer (Relative 1 1))
                >> (dynamicLogWithPP $ myPP xmproc)
    , layoutHook = myLayoutRules
    , manageHook = myManageHook
               <+> manageDocks  
               <+> scratchpadManageHookDefault
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
  , ppHidden = hideScratchPad
  , ppHiddenNoWindows = const ""
  , ppSep = " » "
  , ppTitle = xmobarColor "#7b79b1" "" . shorten 40
  , ppUrgent = xmobarColor "#f92672" "#0f141f"
  , ppWsSep = " "
  , ppExtras = []
  , ppOutput = hPutStrLn output
  }
  where
    hideScratchPad ws = if ws == "NSP" then "" else ws

myDefaultLayout = (tall ||| full)
  where
    tall = named "T" $ ResizableTall 1 (3/100) (1/2) []
    full = named "F" $ noBorders Full

myLayoutRules = avoidStruts $ myDefaultLayout

myManageHook = composeAll
                 [ className =? "MPlayer" --> (ask >>= doF . W.sink) ]

-- Use C-z as a prefix key, and have all other keys come under it.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((controlMask, xK_z), submap . M.fromList $
      [ ((controlMask, xK_z), jumpToPrevWS)
      , ((0,           xK_z), sendKey controlMask xK_z)
      , ((controlMask, xK_g), return ()) -- do nothing
      , ((0,           xK_g), return ()) -- do nothing

      -- Focusing and swapping windows
      , ((0,         xK_n),  windows W.focusDown)
      , ((0,         xK_p),  windows W.focusUp)
      , ((0,         xK_o),  windows W.focusDown)
      , ((shiftMask, xK_n),  windows W.swapDown)
      , ((shiftMask, xK_p),  windows W.swapUp)
      , ((0,         xK_m),  windows W.focusMaster)
      , ((shiftMask, xK_m),  promote)
      , ((shiftMask, xK_t),  withFocused $ windows . W.sink)
      , ((modm,      xK_c),  kill)

      -- Control Xmonad (restart, quit)
      , ((shiftMask, xK_q),  io (exitWith ExitSuccess))
      , ((0,         xK_q),  spawn "xmonad --recompile && xmonad --restart")
      , ((0,         xK_b),  sendMessage ToggleStruts)

      -- Switching layouts
      , ((0,         xK_space), sendMessage NextLayout)
      , ((shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

      -- Spawning other applications
      , ((0,         xK_t),     spawn $ XMonad.terminal conf)
      ]
      ++

      -- Switch workspaces and move windows to other workspaces
      [((m, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [ (W.greedyView, 0)
                        , (W.shift, shiftMask)
                        , (viewOnScreen 1, controlMask)]]
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
    , ((modm, xK_minus), sendMessage (IncMasterN (-1)))
    , ((modm, xK_equal), sendMessage (IncMasterN 1))

    -- Controlling Music and Volume
    , ((modm, xK_F1),          spawn "mpc prev")
    , ((modm, xK_F2),          spawn "mpc toggle")
    , ((modm, xK_F3),          spawn "mpc next")                              
    , ((modm, xK_Print),       spawn "amixer set Master 5%-")
    , ((modm, xK_Scroll_Lock), spawn "amixer set Master 5%+")
    , ((modm, xK_Pause),       spawn "amixer set Master toggle")

    -- Default screen and workspace configurations
    , ((modm, xK_1),  windows $ viewOnScreen 0 (myWorkspaces!!0) 
                              . viewOnScreen 1 (myWorkspaces!!8))

    -- Activating certain applications/desktops
    , ((modm,     xK_space), scratchpadSpawnAction conf)
    , ((mod4Mask, xK_space), spawn "gmrun")
  ]

jumpToPrevWS :: X ()
jumpToPrevWS = do
    ws <- gets windowset
    let hs = filter ((/="NSP") . W.tag) $ W.hidden ws
    unless (null hs) (windows . W.view . W.tag $ head hs)
