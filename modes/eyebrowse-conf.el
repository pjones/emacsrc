;;; eyebrowse-conf.el -- Settings for eyebrowse.
;;
;;; Commentary:
;;
;;; Code:
(require 'eyebrowse)

(custom-set-variables
 '(eyebrowse-wrap-around t)
 '(eyebrowse-new-workspace t))

(declare-function pjones:plasmoid-update "../lisp/functions.el")
(add-hook 'eyebrowse-post-window-switch-hook #'pjones:plasmoid-update)

;;; eyebrowse-conf.el ends here
