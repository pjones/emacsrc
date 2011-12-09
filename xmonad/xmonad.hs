import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar myConfig
myConfig = defaultConfig
