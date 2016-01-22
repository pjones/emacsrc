;;; css-mode-conf.el -- Settings for css-mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'css-mode)
  (require 'kite-mini))

(declare-function pjones:add-programming-hook "code.el")

(defun pjones:css-mode-hook ()
  "Settings and overrides for css-mode."
  (setq css-indent-offset 2)
  (kite-mini-mode)
  (add-hook 'after-save-hook 'pjones:after-save-reload-browser nil t))

(pjones:add-programming-hook 'css-mode-hook)
(add-hook 'css-mode-hook 'pjones:css-mode-hook)
