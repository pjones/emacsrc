;;; erc-conf.el -- Settings for erc.el
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'erc))

;; Dependencies:
(require 'passmm)
(require 'adaptive-wrap)

(custom-set-variables
 '(erc-nick "pmade")
 '(erc-user-full-name "Peter Jones")
 '(erc-rename-buffers t)
 '(erc-network-hide-list '(("freenode" "JOIN" "PART" "QUIT")))
 '(erc-prompt "❯")
 '(erc-query-display 'buffer)
 '(erc-auto-query 'bury)
 '(erc-track-visibility 'selected-visible)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-shorten-start 4)
 '(erc-track-shorten-cutoff 4)
 '(erc-track-switch-from-erc nil)
 '(erc-track-when-inactive nil)
 '(erc-input-line-position -1)
 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-format-left "[%H:%M] ")
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-insert-away-timestamp-function 'erc-insert-timestamp-left)
 '(erc-timestamp-only-if-changed-flag nil)

 '(erc-modules '(autoaway
                 autojoin
                 button
                 completion
                 irccontrols
                 list
                 match
                 move-to-prompt
                 netsplit
                 networks
                 noncommands
                 notifications
                 readonly
                 ring
                 spelling
                 stamp
                 track
                 truncate)))

(defun pjones:erc-connect (username)
  "Connect to an IRC network via ERC with USERNAME."
  (let ((pass (passmm-get-password "machines/chat.devalot.com/znc")))
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

(defun pjones:erc-mode-hook ()
  "Hook run in new ERC buffers."
  (setq adaptive-wrap-extra-indent 8)
  (visual-line-mode)
  (adaptive-wrap-prefix-mode))

(add-hook 'erc-mode-hook #'pjones:erc-mode-hook)

;;; erc-conf.el ends here
