;;; css-mode-conf.el -- Settings for `css-mode'
;;
;;; Commentary:
;;
;;; Code:

(require 'company)
(require 'css-mode)
(require 'rainbow-mode)

(declare-function pjones:add-programming-hook "code.el")

(custom-set-variables
 '(css-indent-offset 2))

(defun pjones:css-mode-hook ()
  "Settings and overrides for `css-mode'."
  (make-local-variable 'company-backends)
  (push 'company-css (car company-backends)))

(pjones:add-programming-hook 'css-mode-hook)
(add-hook 'css-mode-hook #'pjones:css-mode-hook)
(add-hook 'css-mode-hook #'rainbow-mode)

;;; css-mode-conf.el ends here
