;;; org-mime-conf.el -- Settings for `org-mime' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-mime)

(custom-set-variables
 '(org-mime-library 'mml)
 '(org-mime-export-options
   '(:section-numbers nil
     :with-author nil
     :with-toc nil)))

;;; org-mime-conf.el ends here
