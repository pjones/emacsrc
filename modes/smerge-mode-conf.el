;;; smerge-mode-conf.el -- Settings for `smerge-mode'
;;
;;; Commentary:
;;
;;; Code:
(require 'evil)
(require 'smerge-mode)

(evil-define-key 'normal smerge-mode-map
  (kbd "RET") #'smerge-keep-current
  "S" smerge-basic-map
  "[[" #'smerge-prev
  "]]" #'smerge-next)

;;; smerge-mode-conf.el ends here
