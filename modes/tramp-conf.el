;;; tramp-conf.el --- Settings for TRAMP.
;;
;;; Commentary:
;;; Code:

(require 'tramp)

(custom-set-variables
 '(tramp-terminal-type "dumb")
 '(tramp-default-method "ssh"))

(add-to-list
 'tramp-remote-path
 (concat "/etc/profiles/per-user/" user-login-name "/bin"))

;;; tramp-conf.el ends here
