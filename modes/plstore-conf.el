;;; plstore-conf.el -- Settings for `plstore' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'plstore)

(custom-set-variables
 '(plstore-select-keys 'silent)
 `(plstore-encrypt-to ,epa-file-encrypt-to))

;;; plstore-conf.el ends here
