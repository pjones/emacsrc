;;; erc-conf.el -- Settings for erc.el -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies:

(require 'erc)
(require 'erc-track)
(require 'jinx)
(require 'notifications)

(defun pjones:erc-mode-hook ()
  "Hook run in new ERC buffers."
  (setq-local scroll-conservatively 1000
              visual-wrap-extra-indent 8)
  (visual-line-mode)
  (visual-wrap-prefix-mode)
  (jinx-mode 1))

(defun pjones:erc-ignore-channel ()
  "Disable ERC tracking for channels."
  (interactive)
  (when-let* ((this-channel (or (erc-default-target)
			        (buffer-name (current-buffer))))
              ((and (string-match-p "^#" this-channel)
                    (not (string-match-p "bitlbee" this-channel)))))
    (add-to-list 'erc-track-exclude this-channel)))

(defun pjones:erc-connect (network)
  "Connect to the given IRC NETWORK."
  (interactive
   (list (completing-read "Network: " '("libera" "bitlbee"))))
  (let ((pass (string-trim-right (shell-command-to-string "rbw get znc"))))
    (erc :server (format "%s.freerangebits.com" network)
         :port 6667
         :user "pjones"
         :password (format "pjones/%s:%s" network pass))))

(custom-set-variables
 '(erc-nick "devalot")
 `(erc-user-full-name ,user-full-name)
 '(erc-rename-buffers nil)
 '(erc-prompt "❯")
 '(erc-send-whitespace-lines t)
 '(erc-join-buffer 'buffer)
 '(erc-query-display 'buffer)
 '(erc-auto-query 'bury)
 '(erc-auto-reconnect-display 'buffer)
 `(erc-notifications-icon ,notifications-application-icon)

 '(erc-track-visibility 'selected-visible)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-shorten-start 4)
 '(erc-track-shorten-cutoff 4)
 '(erc-track-switch-from-erc nil)
 '(erc-track-when-inactive nil)
 '(erc-track-position-in-mode-line t)
 '(erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                             "324" "329" "332" "333" "353" "477"))

 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-format-left "[%H:%M] ")
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-insert-away-timestamp-function 'erc-insert-timestamp-left)
 '(erc-server-auto-reconnect nil)
 '(erc-timestamp-only-if-changed-flag nil)

 '(erc-modules
   '(autojoin button completion nicks irccontrols
     keep-place list match move-to-prompt netsplit networks
     noncommands notifications readonly ring stamp track))

 '(erc-network-hide-list '(("irc.freerangebits.com" "JOIN" "PART" "QUIT")))
 '(erc-autojoin-channels-alist '(("irc.freerangebits.com"
                                  "#emacs"
                                  "#human-emacs"
                                  "#nixos"))))

(custom-set-faces
 '(erc-timestamp-face ((t (:foreground nil :inherit 'org-agenda-date-today))))
 '(erc-input-face ((t (:foreground nil :inherit 'default))))
 '(erc-my-nick-face ((t (:foreground nil :inherit 'font-lock-constant-face)))))

(add-function
 :after after-focus-change-function
 (lambda (&rest _args) (erc-modified-channels-update)))

(add-hook 'erc-mode-hook #'pjones:erc-mode-hook)
(add-hook 'erc-track-mode-hook #'pjones:erc-ignore-channel)

;;; erc-conf.el ends here
