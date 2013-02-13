;;; haskell-mode-conf.el -- Settings for Haskell mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'haskell-indent)
  (require 'haskell-mode))

(setq haskell-indent-offset 2)
(pjones:add-programming-hook 'haskell-mode-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
