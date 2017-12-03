;;; magit-conf.el -- Customizations for magit.
(eval-when-compile
  (defvar magit-last-seen-setup-instructions)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (require 'magit))

(custom-set-variables
 '(magit-popup-use-prefix-argument 'default)
 '(magit-status-margin '(t age magit-log-margin-width nil 18))
 '(magit-status-show-hashes-in-headers t)
 '(magit-restore-window-configuration t)
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-display-buffer-function (quote magit-display-buffer-fullframe-status-topleft-v1)))
