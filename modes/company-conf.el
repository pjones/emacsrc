;;; company-conf.el --- company-mode configuration.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'company))

;; Settings for company-mode:
(custom-set-variables
  '(company-show-numbers t)
  '(company-selection-wrap-around t)
  '(company-lighter-base "")
  '(company-backends
     (quote (company-ghc
             company-bbdb
             company-nxml
             company-css
             company-clang
             company-capf
             (company-dabbrev-code company-gtags company-etags company-keywords)
             company-files company-dabbrev))))

;; Enable fuzzy (flx) matching in company-mode.
(company-flx-mode +1)
