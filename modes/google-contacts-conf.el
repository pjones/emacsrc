;;; google-contacts-conf.el -- Settings for Google Contacts
;;
;;; Commentary:
;;
;; Overwrite Google OAuth settings instead of using the public set in
;; google-contacts.el.
;;
;;; Code:
(require 'google-contacts)
(require 'auth-source-pass)

(setq
 google-contacts-oauth-client-id
 (auth-source-pass-get "emacs_client_id" "services/google")

 google-contacts-oauth-client-secret
 (auth-source-pass-get "emacs_client_secret" "services/google"))

;;; google-contacts-conf.el ends here
