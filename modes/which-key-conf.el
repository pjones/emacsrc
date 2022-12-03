;;; which-key-conf.el -- Settings for which-key-mode
;;
;;; Commentary:
;;
;;; Code:
(require 'which-key)
(require 'which-key-posframe)

(custom-set-variables
 '(which-key-idle-delay 2.0)
 '(which-key-idle-secondary-delay 0.1)
 '(which-key-show-docstrings t)
 '(which-key-max-description-length 42)
 '(which-key-show-remaining-keys t))

(which-key-posframe-mode)

;;; which-key-conf.el ends here
