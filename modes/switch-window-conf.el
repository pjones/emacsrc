;;; switch-window-conf.el -- Settings for switch-window.
(eval-when-compile
  (require 'switch-window))

(custom-set-variables
 '(switch-window-shortcut-style 'qwerty)
 '(switch-window-increase 8)
 '(switch-window-timeout 5)
 '(switch-window-threshold 2)
 '(switch-window-preferred 'helm)
 '(switch-window-configuration-change-hook-inhibit t))

;;; switch-window-conf.el ends here
