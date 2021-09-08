;;; python-conf.el -- Settings for `python' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'python)
(require 'reformatter)

(custom-set-variables
 '(python-indent-offset 4))

(reformatter-define python-format
  :program "black"
  :args '("-")
  :group 'python)

(add-hook 'python-mode-hook #'python-format-on-save-mode)

;;; python-conf.el ends here
