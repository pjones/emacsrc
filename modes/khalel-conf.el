;;; khalel-conf.el -- Settings for `khalel' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'khalel)
(require 'org)

(custom-set-variables
 '(khalel-default-alarm "15")
 '(khalel-default-calendar "Peter")
 '(khalel-import-org-file-read-only nil)
 '(khalel-import-org-file-confirm-overwrite nil)
 '(khalel-import-org-file (concat pjones:org-notes-directory
                                  "gtd/calendar.org")))

;;; khalel-conf.el ends here
