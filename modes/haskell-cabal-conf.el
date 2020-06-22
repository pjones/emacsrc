;;; haskell-cabal-conf.el -- Settings for `haskell-cabal'
;;
;;; Commentary:
;;
;;; Code:
(require 'haskell-mode)
(require 'haskell-cabal)
(require 'reformatter)
(require 'evil)

(reformatter-define cabal-format
  :program "cabal-fmt")

(evil-define-key 'normal haskell-cabal-mode-map
  "]]" #'haskell-cabal-next-section
  "[[" #'haskell-cabal-previous-section)

(add-hook 'haskell-cabal-mode-hook #'cabal-format-on-save-mode)
(add-hook 'haskell-cabal-mode-hook #'pjones:prog-mode-hook)

;;; haskell-cabal-conf.el ends here
