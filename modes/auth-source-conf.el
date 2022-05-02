;;; auth-source-conf.el -- Settings for `auth-source' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'auth-source)

(custom-set-variables
 '(auth-sources '(password-store)))     ; Use pass(1) for passwords.

(auth-source-pass-enable)

;;; auth-source-conf.el ends here
