;;; winum-conf.el -- Settings for `winum'
;;
;;; Commentary:
;;
;;; Code:
(require 'winum)

(custom-set-variables
  '(winum-scope 'frame-local))

(add-to-list 'winum-ignored-buffers "*org-roam*")
(add-to-list 'winum-ignored-buffers-regexp "^ *Treemacs-")

;;; winum-conf.el ends here
