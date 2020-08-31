;;; transient-conf.el -- Settings for `transient'
;;
;;; Commentary:
;;
;;; Code:
(require 'transient)

;; Yes transient mode, the escape key means quit!
(define-key transient-base-map (kbd "ESC") nil)
(define-key transient-base-map (kbd "<escape>") #'transient-quit-one)

;;; transient-conf.el ends here
