;;; sh-script-conf.el -- Settings for sh-mode. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'reformatter)
(require 'sh-script)

(custom-set-variables
 '(sh-basic-offset 2)
 '(sh-shell-file "bash"))

(reformatter-define sh-format
  :program "shfmt"
  :args '("-i" "2")
  :group 'sh-mode)

(defun pjones:sh-mode-hook ()
  "Set up `sh-mode-hook' buffers."
  (sh-format-on-save-mode))

(add-to-list 'sh-mode-hook #'pjones:sh-mode-hook)
(add-to-list 'bash-ts-mode-hook #'pjones:sh-mode-hook)

;;; sh-script-conf.el ends here
