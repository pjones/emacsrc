;;; ruby-mode-conf.el -- Settings for `ruby-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ruby-mode)

(custom-set-variables
 '(ruby-deep-arglist 'space)
 '(ruby-deep-indent-paren 'space))

(defun pjones:ruby-mode-hook ()
  "Hook for `ruby-mode'."
  (when (fboundp 'ruby-indent-line)
    (setq indent-line-function 'ruby-indent-line))
  (electric-indent-local-mode))

(add-hook 'ruby-mode-hook #'superword-mode)
(add-hook 'ruby-mode-hook #'pjones:ruby-mode-hook)

;;; ruby-mode-conf.el ends here
