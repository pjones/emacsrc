;;; haskell-cabal-conf.el -- Settings for `haskell-cabal'
;;
;;; Commentary:
;;
;;; Code:

(require 'haskell-mode)
(require 'haskell-cabal)
(require 'reformatter)

(reformatter-define cabal-format
  :program "cabal-fmt"
  :group 'haskell)

(add-hook 'haskell-cabal-mode-hook #'cabal-format-on-save-mode)
(when (fboundp 'pjones:prog-mode-hook)
  (add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook))

;;; haskell-cabal-conf.el ends here
