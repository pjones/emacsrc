;;; which-key-conf.el -- Settings for which-key-mode
;;
;;; Commentary:
;;
;;; Code:
(require 'which-key)

(custom-set-variables
 '(which-key-idle-delay 2.0)
 '(which-key-idle-secondary-delay 0.1)
 '(which-key-show-docstrings t);;'docstring-only)
 '(which-key-max-description-length 42)
 '(which-key-show-remaining-keys t)
 '(which-key-paging-key "<down>")
 '(which-key-popup-type 'side-window)
 '(which-key-side-window-location 'right)
 '(which-key-side-window-max-width 0.5)
 '(which-key-side-window-max-height 0.5)
 '(which-key-show-prefix 'top))

;;; which-key-conf.el ends here
