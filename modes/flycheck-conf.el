;;; flycheck-conf.el -- Configuration for flycheck.
;;
;;; Commentary:
;;
;;; Code:
(require 'flycheck)

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-gjslint))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-standard-error-navigation nil))

;;; flycheck-conf.el ends here
