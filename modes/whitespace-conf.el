;;; whitespace.el -- Configuration options for whitespace-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'whitespace)

(custom-set-variables
 '(whitespace-style '(face tabs newline lines-tail trailing))
 '(whitespace-action '(auto-cleanup)))

;;; whitespace-conf.el ends here
