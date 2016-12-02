;;; flycheck-conf.el -- Configuration for flycheck.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'flycheck))

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-eslint javascript-gjslint))
 '(flycheck-standard-error-navigation nil))
