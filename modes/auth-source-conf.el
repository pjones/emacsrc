;;; auth-source-conf.el -- Settings for `auth-source' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'auth-source)

(custom-set-variables
 '(auth-sources '("~/keys/emacs/authinfo.gpg"
                  password-store)))

(auth-source-pass-enable)

;;; auth-source-conf.el ends here
