;;; flycheck-conf.el -- Configuration for flycheck.
;;
;;; Commentary:
;;
;;; Code:
(require 'flycheck)

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-gjslint))
 '(flycheck-standard-error-navigation nil))

;;; flycheck-conf.el ends here
