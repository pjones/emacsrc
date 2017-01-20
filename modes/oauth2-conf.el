;;; oauth2-conf.el -- OAuth Configuration.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'oauth2))

(custom-set-variables
 `(oauth2-token-file ,(expand-file-name "~/keys/emacs/oauth2.plstore")))
