;;; company-ghc-conf.el --- company-mode ghc-mod backend.
(eval-when-compile
  (require 'company-ghc))

(setq company-ghc-show-info t
      ghc-command "nix-hs-ghc-mod")
