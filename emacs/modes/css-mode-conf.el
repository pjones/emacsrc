;;; css-mode-conf.el -- Settings for css-mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'css-mode))

(defun pjones:css-mode-hook ()
  "Settings and overrides for css-mode."
  (setq css-indent-offset 2))

(pjones:add-programming-hook 'css-mode-hook)
(add-hook 'css-mode-hook 'pjones:css-mode-hook)
