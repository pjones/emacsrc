;;; flycheck-conf.el -- Configuration for flycheck.
;;
;;; Commentary:
;;
;;; Code:
(require 'flycheck)
(require 'flycheck-indicator)

(custom-set-variables
 '(flycheck-disabled-checkers '(javascript-gjslint))
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(flycheck-standard-error-navigation nil)

 '(flycheck-indicator-icon-error 9632)
 '(flycheck-indicator-icon-info 9679)
 '(flycheck-indicator-icon-warning 9650)
 '(flycheck-indicator-status-icons
   '((running . "◉")
     (errored . "◙")
     (finished . "●")
     (interrupted . "◘")
     (suspicious . "◘")
     (not-checked . "○"))))

(add-hook 'flycheck-mode-hook #'flycheck-indicator-mode)

;;; flycheck-conf.el ends here
