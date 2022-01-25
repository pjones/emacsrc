;;; css-mode-conf.el -- Settings for `css-mode'
;;
;;; Commentary:
;;
;;; Code:

(require 'css-mode)
(require 'rainbow-mode)

(declare-function pjones:add-programming-hook "code.el")

(custom-set-variables
 '(css-indent-offset 2))

(pjones:add-programming-hook 'css-mode-hook)
(add-hook 'css-mode-hook #'rainbow-mode)

;;; css-mode-conf.el ends here
