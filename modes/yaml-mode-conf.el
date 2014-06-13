;;; yaml-mode-conf.el -- Settings for yaml-mode.
(eval-when-compile
  (load "../lisp/packages.el")
  (load "../lisp/code.el")
  (require 'yaml-mode))

(declare-function pjones:add-programming-hook "code.el")

(defun pjones:yaml-new-array-item ()
  "Make yaml-mode sort of like org-mode by inserting a newline,
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
(pjones:add-programming-hook 'yaml-mode-hook)
