;;; switch-window-conf.el -- Settings for switch-window.
;;
;;; Commentary:
;;
;;; Code:
(require 'switch-window)

(custom-set-variables
 '(switch-window-shortcut-style 'qwerty)
 '(switch-window-minibuffer-shortcut ?z)
 '(switch-window-timeout 5)
 '(switch-window-threshold 2)
 '(switch-window-preferred 'ivy)
 '(switch-window-configuration-change-hook-inhibit t)
 '(switch-window-input-style 'minibuffer))

;;; switch-window-conf.el ends here
