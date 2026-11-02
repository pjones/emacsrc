;;; cdsync-conf.el -- Settings for `cdsync' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cdsync)

(custom-set-variables
 '(cdsync-auth-source-host "emacs-caldav")
 '(cdsync-calendars-file (expand-file-name "~/notes/calendars/cdsync.eld")))

;;; cdsync-conf.el ends here
