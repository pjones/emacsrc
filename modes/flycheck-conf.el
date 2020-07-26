;;; flycheck-conf.el -- Configuration for flycheck.
;;
;;; Commentary:
;;
;;; Code:
(require 'flycheck)
(require 'flycheck-posframe)

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-gjslint))
 '(flycheck-standard-error-navigation nil)
 '(flycheck-posframe-info-prefix "• ")
 '(flycheck-posframe-warning-prefix "• ")
 '(flycheck-posframe-error-prefix "• "))

(add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)

;;; flycheck-conf.el ends here
