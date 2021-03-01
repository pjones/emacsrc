;;; man-conf.el -- Settings for `man'
;;
;;; Commentary:
;;
;;; Code:
(require 'man)
(require 'evil)

(custom-set-variables
 '(Man-notify-method 'pushy))

(evil-define-key 'normal Man-mode-map
  (kbd "TAB") #'forward-button
  (kbd "<return>") #'push-button)

;;; man-conf.el ends here
