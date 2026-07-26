;;; erc-conf.el -- Settings for erc.el -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies:

(require 'erc)
(require 'erc-track)
(require 'notifications)

;; Make the linting tool happy:
(defvar visual-wrap-extra-indent)

(defvar pjones:erc-modified-channels-alist nil
  "A cache of `erc-modified-channels-alist'.")

(defun pjones:erc-mode-hook ()
  "Hook run in new ERC buffers."
  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 1000      ; Don't recenter window
        visual-wrap-extra-indent 8)   ; Leave space for timestamp.
  (visual-line-mode)
  (visual-wrap-prefix-mode))

(defun pjones:erc-ignore-channel ()
  "Disable ERC tracking for channels."
  (interactive)
  (when (and (erc-default-target)
             (string-match-p "^#" (erc-default-target)))
    (add-to-list 'erc-track-exclude (erc-default-target))))

(defun pjones:erc-connect ()
  "Connect to irc."
  (interactive)
  (erc :server "irc.freerangebits.com"
       :port 6667
       :user "pjones"))

(custom-set-variables
 '(erc-nick "devalot")
 `(erc-user-full-name ,user-full-name)
 '(erc-rename-buffers nil)
 '(erc-prompt "❯")
 '(erc-query-display 'buffer)
 '(erc-auto-query 'bury)
 `(erc-notifications-icon ,notifications-application-icon)
 '(erc-track-visibility 'selected-visible)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-shorten-start 4)
 '(erc-track-shorten-cutoff 4)
 '(erc-track-switch-from-erc nil)
 '(erc-track-when-inactive nil)
 '(erc-track-position-in-mode-line nil)
 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-format-left "[%H:%M] ")
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-insert-away-timestamp-function 'erc-insert-timestamp-left)
 '(erc-server-auto-reconnect nil)
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-modules '(autojoin button completion hl-nicks irccontrols
                 list match move-to-prompt netsplit networks noncommands
                 notifications readonly ring spelling stamp track))
 '(erc-network-hide-list '(("Libera.Chat" "JOIN" "PART" "QUIT")))
 '(erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                             "324" "329" "332" "333" "353" "477"))
 '(erc-autojoin-channels-alist '((irc.libera.chat:6697 "#emacs" "#human-emacs"))))


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
