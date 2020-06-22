;;; comint-conf.el -- Settings for `comint'
;;
;;; Commentary:
;;
;;; Code:
(require 'comint)

(define-key comint-mode-map (kbd "C-k") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-j") #'comint-next-input)

;;; comint-conf.el ends here
