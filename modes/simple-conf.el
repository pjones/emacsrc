;;; simple-conf.el -- Functions and settings for simple.el (mostly for
;;
;;; Commentary:
;;
;;; Code:

(require 'simple)

(custom-set-variables
 '(async-shell-command-buffer 'new-buffer)
 '(cycle-spacing-actions '((just-one-space -) (delete-all-space -) restore))
 '(kill-do-not-save-duplicates t)
 '(next-error-highlight 1)
 '(next-error-message-highlight 'keep)
 '(next-error-recenter '(4))
 '(next-line-add-newlines t)
 '(set-mark-command-repeat-pop t)
 '(shell-command-dont-erase-buffer 'end-last-out))

;;; simple-conf.el ends here
