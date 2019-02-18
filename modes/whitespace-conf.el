;;; whitespace.el -- Configuration options for whitespace-mode.
;;
;;; Commentary:
;;
;;; Code:
(require 'whitespace)
(require 'diminish)

(custom-set-variables
 '(whitespace-style '(face tabs newline lines-tail))
 '(whitespace-action '(auto-cleanup)))

(defun pjones:whitespace-mode-hook ()
  "Hook for `whitespace-mode-hook'."
  (diminish 'whitespace-mode " ␣"))

(add-hook 'whitespace-mode-hook #'pjones:whitespace-mode-hook)

;;; whitespace-conf.el ends here
