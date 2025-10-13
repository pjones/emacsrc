;;; citar-conf.el -- Settings for `citar' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'citar)
(require 'org) ; For `org-cite-global-bibliography'

(declare-function citar-embark-mode "citar-embark")

(custom-set-variables
 '(citar-bibliography org-cite-global-bibliography)
 '(citar-open-entry-function #'citar-open-entry-in-zotero)
 '(citar-at-point-function #'embark-act))

;; Other modes to start:
(citar-embark-mode)

;;; citar-conf.el ends here
