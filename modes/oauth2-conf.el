;;; oauth2-conf.el -- Settings for OAuth2
;;
;;; Commentary:
;;
;;; Code:
(require 'oauth2)

(custom-set-variables
 `(oauth2-token-file
   ,(expand-file-name "~/.password-store/emacs/oauth2.plstore")))

;;; oauth2-conf.el ends here
