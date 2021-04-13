;;; haskell-cabal-conf.el -- Settings for `haskell-cabal'
;;
;;; Commentary:
;;
;;; Code:

(require 'haskell-mode)
(require 'haskell-cabal)
(require 'reformatter)

(declare-function pjones:prog-mode-hook "../lisp/code.el")

(reformatter-define cabal-format
  :program "cabal-fmt"
  :group 'haskell)

(add-hook 'haskell-cabal-mode-hook #'cabal-format-on-save-mode)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)

;;; haskell-cabal-conf.el ends here
