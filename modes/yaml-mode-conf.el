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
  "Fix a few weird things about yaml-mode."
  ;; reindent-then-newline-and-indent doesn't work very well in
  ;; yaml-mode :(
  (local-set-key (kbd "RET")        'newline-and-indent)
  (local-set-key (kbd "C-<return>") 'newline)
  (local-set-key (kbd "M-RET")      'pjones:yaml-new-array-item))

(add-hook 'yaml-mode-hook 'pjones:yaml-mode-hook)
(when (fboundp 'pjones:prog-mode-hook)
  (add-hook 'yaml-mode-hook #'pjones:prog-mode-hook))


;;; yaml-mode-conf.el ends here
