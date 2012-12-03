-- Links:
--   xmonad: http://xmonad.org/
--   xmonad-contrib: http://xmonad.org/xmonad-docs/xmonad-contrib/

-- Load XMonad
import XMonad
import qualified XMonad.StackSet as W

-- XMonad contrib (Prompt)
import XMonad.Prompt
import XMonad.Prompt.Window (windowPromptGoto)
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Prompt.Shell (shellPrompt)

-- XMonad contrib (Actions)
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Actions.UpdatePointer
import XMonad.Actions.OnScreen (viewOnScreen, greedyViewOnScreen)
import XMonad.Actions.PerWorkspaceKeys (bindOn)

-- XMonad contrib (Hooks)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops

-- XMonad contrib (Layouts)
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named (named)
import XMonad.Layout.BoringWindows
import qualified XMonad.Layout.PerWorkspace as LPW
import XMonad.Layout.SimplestFloat (simplestFloat)

-- Utilities
import qualified Data.Map as M
import Data.Ratio
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import Control.Monad (when, unless)
import Data.Monoid
import XMonad.Util.Types
import XMonad.Util.Run
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Scratchpad

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ ewmh $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { modMask = mod3Mask
    , terminal = "urxvtc"
    , borderWidth = 3
    , normalBorderColor = "#1a1a1a"
    , focusedBorderColor = "#00bfff"
    , focusFollowsMouse = False
    , workspaces = myWorkspaces
    , keys = myKeys
    , logHook = fadeWindowsLogHook myFadeHook
                >> (updatePointer (Relative 0.98 0.01))
                >> (dynamicLogWithPP $ myPP xmproc)
    , layoutHook = myLayoutRules
    , manageHook = myManageHook
               <+> manageDocks
               <+> scratchpadManageHookDefault
    , handleEventHook = focusFollowsTiledOnly <+> fadeWindowsEventHook
    }


-- | Enables 'focusFollowsMouse' for tiled windows only.  For this to
--   work you need to turn off 'focusFollowsMouse' in your
--   configuration and then add this function to your
--   'handleEventHook'.
focusFollowsTiledOnly :: Event -> X All
focusFollowsTiledOnly e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode e == notifyNormal
    = do whenX (gets (not . M.member w . W.floating . windowset)) (focus w)
         return $ All True
focusFollowsTiledOnly _ =  return $ All True

myWorkspaces :: [String]
myWorkspaces = map show ([1..9] ++ [0])

myPP output = defaultPP
  { ppCurrent         = xmobarColor "#88b324" "" . wrap "[" "]"
  , ppVisible         = wrap "(" ")"
  , ppHidden          = hideScratchPad
  , ppHiddenNoWindows = xmobarColor "#3c3c3c" "" . hideScratchPad
  , ppSep             = xmobarColor "#5c5c5c" "" " | "
  , ppTitle           = xmobarColor "#9396c4" "" . shorten 40
  , ppUrgent          = xmobarColor "#1c1c1c" "#d33682" . wrap "{" "}"
  , ppWsSep           = " "
  , ppExtras          = []
  , ppOutput          = hPutStrLn output
  }
  where
    hideScratchPad "NSP" = ""
    hideScratchPad "P1"  = ""  -- Temp fix until I restart xmonad
    hideScratchPad "P2"  = ""  -- ditto.
    hideScratchPad name  = name

myDefaultLayout =
  onWorkspace "8" float $
  boringWindows (tall ||| full)
  where
    tall  = named "T"  $ ResizableTall 1 (1.5/100) (2/3) []
    full  = named "F"  $ noBorders Full
    float = named "FL" $ simplestFloat

myLayoutRules = avoidStruts $ myDefaultLayout

myManageHook = composeAll
  [ className =? "MPlayer"    --> (ask >>= doF . W.sink)
  , title     =? "HandBrake"  --> (ask >>= doF . W.sink)
  , appName   =? "random-vnc" --> doShift "P1"
  ]

myFadeHook = composeAll
  [ transparency 0.02  --  Default
  , isUnfocused        --> transparency 0.1
  , isFloating         --> opaque
  , className =? "feh" --> opaque
  , className =? "vlc" --> opaque
  ]

-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Prompt.html#t:XPConfig
myXPConfig = defaultXPConfig
  { position = Bottom
  , font = "xft:dejavu sans mono:size=9"
--  , bgColor = "#1c1c1c"
--  , fgColor = "#e6e6e6"
--  , bgHLight = ""
--  , fgHLight = ""
--  , promptBorderWidth = 2
--  , borderColor = ""
  }

-- Use C-z as a prefix key, and have all other keys come under it.
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ((controlMask, xK_z), submap . M.fromList $
      [ ((controlMask, xK_z), jumpToPrevWS)
      , ((0,           xK_z), sendKey controlMask xK_z)
      , ((controlMask, xK_g), return ()) -- do nothing
      , ((0,           xK_g), return ()) -- do nothing

      -- Focusing and swapping windows
      , ((0,         xK_n),  focusDown) -- windows W.focusDown)
      , ((0,         xK_p),  focusUp)   -- windows W.focusUp)
      , ((0,         xK_o),  focusDown) -- windows W.focusDown)
      , ((shiftMask, xK_n),  windows W.swapDown)
      , ((shiftMask, xK_p),  windows W.swapUp)
      , ((0,         xK_m),  focusMaster) -- windows W.focusMaster)
      , ((shiftMask, xK_m),  promote)
      , ((shiftMask, xK_t),  withFocused $ windows . W.sink)
      , ((shiftMask, xK_k),  kill)
      , ((0,         xK_w),  windowPromptGoto myXPConfig)

      -- Control Xmonad (restart, quit)
      , ((shiftMask, xK_q),  io (exitWith ExitSuccess))
      , ((0,         xK_q),  spawn "xmonad --recompile && xmonad --restart")
      , ((0,         xK_s),  sendMessage ToggleStruts)
      , ((0,         xK_x),  xmonadPrompt myXPConfig)

      -- Switching layouts
      , ((0,         xK_space), sendMessage NextLayout)
      , ((shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

      -- Spawning other applications
      , ((0,           xK_t),     spawn $ XMonad.terminal conf)
      , ((controlMask, xK_t),     spawn "urxvtc -name BigTerm")
      , ((0,           xK_l),     jumpToPrevWS)
      ]

      ++
      -- Switch workspaces and move windows to other workspaces, but
      -- disable this feature on worksapces P1 and P2.  Also, don't
      -- allow switching to workspaces P1 or P2 directly.
      [((m, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
            , (f, m) <- [ (W.greedyView, 0)
                        , (W.shift, shiftMask)]]
      ++
      -- Switch which workspace is on the second physical screen
      [((m, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
            , (f, m) <- [(viewOnScreen 1, controlMask)]]
      ++
      -- Switch screens and move workspaces to other screens
      [((m, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_b, xK_f] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]])

    -- Remaining keys that use the XMonad modifier key

    -- Window sizing and control
    , ((modm, xK_Left),  sendMessage Shrink)
    , ((modm, xK_Right), sendMessage Expand)
    , ((modm, xK_Up),    sendMessage MirrorShrink)
    , ((modm, xK_Down),  sendMessage MirrorExpand)
    , ((modm, xK_minus), sendMessage (IncMasterN (-1)))
    , ((modm, xK_equal), sendMessage (IncMasterN 1))
    , ((modm, xK_b),     markBoring)

    -- Controlling Music and Volume
    , ((modm, xK_F1),     spawn "mpc-pause")
    , ((modm, xK_F2),     spawn "mpc prev")
    , ((modm, xK_F3),     spawn "mpc next")
    , ((mod4Mask, xK_F1), spawn "amixer set Master toggle")
    , ((mod4Mask, xK_F2), spawn "amixer set Master 5%-")
    , ((mod4Mask, xK_F3), spawn "amixer set Master 5%+")

    -- Same actions, but for my laptop
    , ((0, xF86XK_AudioMute),           spawn "amixer set Master toggle")
    , ((0, xF86XK_AudioLowerVolume),    spawn "amixer set Master 5%-")
    , ((0, xF86XK_AudioRaiseVolume),    spawn "amixer set Master 5%+")
    , ((modm, xF86XK_AudioMute),        spawn "mpc-pause")
    , ((modm, xF86XK_AudioLowerVolume), spawn "mpc prev")
    , ((modm, xF86XK_AudioRaiseVolume), spawn "mpc next")

    -- Other actions exclusively for my laptop
    , ((0, xF86XK_WebCam), spawn "tptoggle.sh")

    -- Activating certain applications/desktops
    , ((modm,     xK_space), scratchpadSpawnAction conf)
    , ((mod4Mask, xK_space), shellPrompt myXPConfig)
    , ((modm,     xK_l),     spawn "xscreensaver-command -lock")
    , ((0,    xK_Print),     spawn "screenshot.sh root")
    , ((modm, xK_Print),     spawn "screenshot.sh window")
  ]

jumpToPrevWS :: X ()
jumpToPrevWS = do
    ws <- gets windowset
    let hs = filter ((/="NSP") . W.tag) $ W.hidden ws
    unless (null hs) (windows . W.view . W.tag $ head hs)
