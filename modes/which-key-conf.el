;;; which-key-conf.el -- Settings for which-key-mode
;;
;;; Commentary:
;;
;;; Code:
(require 'which-key)
(require 'diminish)

(custom-set-variables
 '(which-key-idle-delay 2.0)
 '(which-key-show-docstrings 'docstring-only)
 '(which-key-max-description-length 42)
 '(which-key-side-window-max-height 0.5)
 '(which-key-show-remaining-keys t)
 '(which-key-paging-key "<down>"))

(defun pjones:which-key-mode-hook ()
  "Hook for `which-key-mode'."
  (diminish 'which-key-mode))

(add-hook 'which-key-mode-hook #'pjones:which-key-mode-hook)

;;; which-key-conf.el ends here
