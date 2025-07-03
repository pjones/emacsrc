;;; yaml-mode-conf.el -- Settings for `yaml-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'yaml-mode)

(defun pjones:yaml-new-array-item ()
  "Insert a new array item.
Make `yaml-mode' sort of like `org-mode' by inserting a newline,
indenting, and then inserting the array marker (dash)."
  (interactive)
  (newline-and-indent)
  (insert "- "))

(defun pjones:yaml-mode-hook ()
  "Set up `yaml-mode' buffers."
  (when (fboundp 'pjones:prog-mode-hook)
    (pjones:prog-mode-hook))
  (setq-local indent-line-function #'yaml-indent-line)
  (local-set-key (kbd "M-RET") 'pjones:yaml-new-array-item))

(add-hook 'yaml-mode-hook 'pjones:yaml-mode-hook)
(add-hook 'yaml-ts-mode-hook 'pjones:yaml-mode-hook)

;;; yaml-mode-conf.el ends here
