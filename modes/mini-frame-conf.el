;;; mini-frame-conf.el -- Settings for `mini-frame' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'mini-frame)

(custom-set-variables
 '(mini-frame-show-parameters
   '((top . 0.25)
     (width . 0.85)
     (left . 0.5)
     (internal-border-width . 5)))
 '(mini-frame-internal-border-color "#000000"))

(add-to-list 'mini-frame-ignore-functions 'notmuch-jump)

;;; mini-frame-conf.el ends here
