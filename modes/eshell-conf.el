;;; eshell-conf.el -- Settings for Eshell
(eval-when-compile
  (require 'eshell))

(require 'esh-mode)

(custom-set-variables
 '(eshell-prompt-regexp "^❯ ")
 '(eshell-review-quick-commands nil)
 '(eshell-where-to-jump (quote after))
 '(eshell-smart-space-goes-to-end t))

(defun pjones:eshell-mode-hook ()
  "Customize Eshell."
  (require 'em-smart)
  (eshell-smart-initialize))

(defvar eshell-mode-hook "Why isn't this defined?" nil)
(add-to-list 'eshell-mode-hook #'pjones:eshell-mode-hook)
