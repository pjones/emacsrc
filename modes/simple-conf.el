;;; simple-conf.el -- Functions and settings for simple.el (mostly for
;;
;;; Commentary:
;;
;;; Code:

(require 'simple)

(declare-function pjones:prog-mode-hook "../lisp/code.el")

(custom-set-variables
 '(shell-command-dont-erase-buffer 'end-last-out)
 '(async-shell-command-buffer 'new-buffer)
 '(kill-do-not-save-duplicates t)
 '(set-mark-command-repeat-pop t)
 '(next-line-add-newlines nil)
 '(next-error-highlight 1)
 '(next-error-recenter '(4)))

(add-hook 'prog-mode-hook #'pjones:prog-mode-hook)

;;; simple-conf.el ends here
