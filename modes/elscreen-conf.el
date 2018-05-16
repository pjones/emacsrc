;;; elscreen-conf.el -- Settings for elscreen.el
(eval-when-compile
  (require 'elscreen))

(custom-set-variables
 `(elscreen-prefix-key ,(kbd "s-z"))
 '(elscreen-default-buffer-initial-message nil)
 '(elscreen-display-tab nil)
 '(elscreen-tab-display-control nil))
