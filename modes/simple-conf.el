;;; simple-conf.el -- Functions and settings for simple.el (mostly for
;;
;;; Commentary:
;;
;;; Code:
(declare-function pjones:prog-mode-hook "../lisp/code.el")

(custom-set-variables
 '(shell-command-dont-erase-buffer 'end-last-out))

(add-hook 'prog-mode-hook #'pjones:prog-mode-hook)

;;; simple-conf.el ends here
