;;; man-conf.el -- Settings for `man'
;;
;;; Commentary:
;;
;;; Code:
(require 'man)
(require 'evil)

(evil-define-key 'normal Man-mode-map
  (kbd "TAB") #'forward-button
  (kbd "<return>") #'push-button)

;;; man-conf.el ends here
