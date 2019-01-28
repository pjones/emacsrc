;;; erc-conf.el -- Settings for erc.el
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies:
(require 'erc)
(require 'erc-track)
(require 'passmm)
(require 'adaptive-wrap)

(custom-set-variables
 '(erc-nick "pmade")
 '(erc-user-full-name "Peter Jones")
 '(erc-rename-buffers nil)
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
 '(erc-server-auto-reconnect nil)
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
                 truncate))

 '(erc-network-hide-list '(("freenode" "JOIN" "PART" "QUIT")))

 '(erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                             "324" "329" "332" "333" "353" "477")))


;; Always ignore the bitlbee control channel.
(add-to-list 'erc-track-exclude "&bitlbee")

(defun pjones:erc-connect (network)
  "Connect to an IRC NETWORK via ERC."
  (let ((pass (passmm-get-password "machines/chat.devalot.com/znc")))
    (erc-tls :server   (format "%s.pmade.com" network)
             :nick     "pjones"
             :port     6697
             :password (format "pjones/%s:%s" network pass))))

(defun pjones:erc-freenode ()
  "Connect to the freenode network."
  (interactive)
  (pjones:erc-connect "freenode"))

(defun pjones:erc-bitlbee ()
  "Connect to the bitlbee network."
  (interactive)
  (pjones:erc-connect "bitlbee"))

(defun pjones:erc-mode-hook ()
  "Hook run in new ERC buffers."
  (setq adaptive-wrap-extra-indent 8)
  (visual-line-mode)
  (adaptive-wrap-prefix-mode))

(defun pjones:erc-ignore-channel ()
  "Disable ERC tracking for channels."
  (interactive)
  (when (and (erc-default-target)
             (string-match-p "^#" (erc-default-target)))
    (add-to-list 'erc-track-exclude (erc-default-target))))

(add-hook 'erc-mode-hook #'pjones:erc-mode-hook)
(add-hook 'erc-track-mode-hook #'pjones:erc-ignore-channel)

;;; erc-conf.el ends here
