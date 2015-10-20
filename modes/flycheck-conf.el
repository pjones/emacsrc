;;; flycheck-conf.el -- Configuration for flycheck.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'flycheck))

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-eslint javascript-gjslint)))

(setq flycheck-standard-error-navigation nil) ; Don't take over next-error!
