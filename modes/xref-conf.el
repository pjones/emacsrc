;;; xref-conf.el -- Settings for `xref' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'xref)
(require 'xref-project-history)

(custom-set-variables
 '(xref-history-storage #'xref-project-history)
 '(xref-search-program 'ripgrep)
 '(xref-show-xrefs-function #'consult-xref)
 '(xref-show-definitions-function #'consult-xref))

;;; xref-conf.el ends here
