;;; haskell-mode-conf.el -- Settings for Haskell mode.
(eval-when-compile
  (load "../lisp/code.el")
  (require 'haskell-simple-indent)
  (require 'haskell-indent)
  (require 'haskell-mode))

(defun pjones:haskell-mode-hook ()
  (pjones:prog-mode-hook)
  (local-set-key (kbd "RET") 'newline-and-indent)
  (make-local-variable 'tab-always-indent)
  (setq tab-always-indent t
        haskell-indent-offset 4)
  (turn-on-haskell-simple-indent))

;;  (turn-on-haskell-indent)

(add-hook 'haskell-mode-hook 'pjones:haskell-mode-hook)
