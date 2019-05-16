;;; flycheck-conf.el -- Configuration for flycheck.
;;
;;; Commentary:
;;
;;; Code:
(require 'flycheck)

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-eslint javascript-gjslint))
 '(flycheck-standard-error-navigation nil)
 '(flymake-no-changes-timeout nil)
 '(flymake-start-syntax-check-on-newline nil)
 '(flycheck-check-syntax-automatically '(save mode-enabled)))

;;; flycheck-conf.el ends here
