;;; plstore-conf.el -- Settings for plstore.el.
(eval-when-compile
  (require 'plstore))

(custom-set-variables
 '(plstore-select-keys 'silent)
 `(plstore-encrypt-to ,epa-file-encrypt-to))
