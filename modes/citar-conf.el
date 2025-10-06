;;; citar-conf.el -- Settings for `citar' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'citar)
(require 'org)

(custom-set-variables
 '(citar-bibliography org-cite-global-bibliography)
 '(citar-open-entry-function citar-open-entry-in-zotero))

;;; citar-conf.el ends here
