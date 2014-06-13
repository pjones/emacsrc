;;; conf-mode-conf.el -- Settings for conf-mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'conf-mode))

(declare-function pjones:add-programming-hook "code.el")
(pjones:add-programming-hook 'conf-mode-hook)
