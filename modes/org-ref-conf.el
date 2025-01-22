;;; org-ref-conf.el -- Settings for `org-ref' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-ref)

(custom-set-variables
 '(org-ref-insert-cite-function
   (lambda () (org-cite-insert nil))))

;;; org-ref-conf.el ends here
