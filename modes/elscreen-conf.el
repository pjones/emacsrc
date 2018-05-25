;;; elscreen-conf.el -- Settings for elscreen.el
(eval-when-compile
  (require 'elscreen))

(custom-set-variables
 `(elscreen-prefix-key ,(kbd "s-z"))
 '(elscreen-default-buffer-initial-message nil)
 '(elscreen-display-tab nil)
 '(elscreen-tab-display-control nil))

;; elscreen loads too late so we need to help it boot:
(elscreen-make-frame-confs (selected-frame))
