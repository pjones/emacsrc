;;; plstore-conf.el -- Settings for plstore.el.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'plstore))

(custom-set-variables
 '(plstore-select-keys 'silent)
 `(plstore-encrypt-to ,epa-file-encrypt-to))
