;;; org-trello-conf.el -- Settings for org-trello
;;
;;; Commentary:
;;
;;; Code:
(require 'org-trello)
(require 'auth-source-pass)

(setq-default
 org-trello-consumer-key
 (auth-source-pass-get "emacs_consumer_key" "services/trello")

 org-trello-access-token
 (auth-source-pass-get "emacs_access_token" "services/trello"))

(defun pjones:orgtrello-controller-list-user-accounts (&optional _)
  "Return a list of usernames."
  (list
   (auth-source-pass-get "username" "services/trello")))
(defalias 'orgtrello-controller-list-user-accounts
  #'pjones:orgtrello-controller-list-user-accounts)

(defun pjones:orgtrello-controller-load-keys (&optional _)
  "No-op since they are stored in memory."
  :ok)
(defalias 'orgtrello-controller-load-keys
  #'pjones:orgtrello-controller-load-keys)

;;; org-trello-conf.el ends here
