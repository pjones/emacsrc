;;; erc-conf.el -- Settings for erc.el
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'erc))

;; Dependencies:
(require 'password-store)

(custom-set-variables
 '(erc-nick "pmade")
 '(erc-user-full-name "Peter Jones")
 '(erc-rename-buffers t)
 '(erc-network-hide-list '(("freenode" "JOIN" "PART" "QUIT")))
 '(erc-prompt "❯")
 '(erc-query-display 'buffer)
 '(erc-auto-query 'bury))

(defun pjones:erc-connect (username)
  "Connect to an IRC network via ERC with USERNAME."
  (let ((pass (password-store-get "machines/chat.devalot.com/znc")))
    (erc-tls :server   "chat.devalot.com"
             :nick     "pjones"
             :port     6697
             :password (format "%s:%s" username pass))))

(defun pjones:erc-freenode ()
  "Connect to the freenode network."
  (interactive)
  (pjones:erc-connect "pjones/freenode"))

(defun pjones:erc-bitlbee ()
  "Connect to the bitlbee network."
  (interactive)
  (pjones:erc-connect "pjones/bitlbee"))

;;; erc-conf.el ends here
