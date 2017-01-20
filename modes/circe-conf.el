;;; circe-conf.el -- Settings for circe.el
(eval-when-compile
  (load "../lisp/packages.el")
  (load "../lisp/functions.el")
  (require 'circe))

(require 'circe-notifications)
(require 'circe-color-nicks)
(require 'tracking)

(defvar freenode-password nil
  "My nick password for Freenode.")

(defvar bitlbee-password nil
  "My password for connecting to Bitlbee.")

(defvar pjones-irc-password nil
  "My password for connecting to ZNC.")

;; Load passwords from a private file
(load "~/keys/emacs/secrets.el")

(custom-set-variables
 '(circe-default-nick "pmade")
 '(circe-default-user "pmade")
 '(circe-default-realname "Peter J. Jones")
 '(circe-reduce-lurker-spam t)
 '(circe-server-auto-join-default-type :after-auth)
 '(circe-format-say "< {nick}: {body}")
 `(circe-prompt-string ,(concat (propertize "❯" 'face 'circe-prompt-face) " "))

 '(circe-notifications-watch-strings '("pjones" "peter"))
 '(circe-notifications-alert-style 'libnotify)
 '(circe-notifications-notify-function 'pjones:circe-notifications-notify-function)

 '(tracking-ignored-buffers '(("^#" circe-highlight-nick-face)))
 '(tracking-faces-priorities '(circe-highlight-nick-face))
 '(tracking-position 'end)

 '(lui-track-bar-behavior 'after-sending)
 '(lui-time-stamp-format "%H:%M")
 '(lui-time-stamp-position 'right-margin)
 '(lui-fill-type nil))

(custom-set-faces
 '(circe-prompt-face ((t (:weight bold :foreground "#2aa198")))))

(setq circe-network-options
  `(("freenode"
     :host "freenode.pmade.com" :port 6697 :use-tls t
     :user "pjones/freenode"    :nick "pmade"
     :pass ,pjones-irc-password
     :channels ("#emacs" "#nixos" "#xmonad" "#haskell"))
    ("bitlbee"
     :host "bitlbee.pmade.com" :port 6697 :use-tls t
     :user "pjones/bitlbee"    :nick "pjones"
     :pass ,pjones-irc-password)))

(defun pjones:circe-notifications-notify-function (nick body channel)
  "Handle notifications from circe.

First set urgency hints on the current frame then pass the NICK,
BODY, and CHANNEL through to the default notification system."
  (let ((circe-notifications-notify-function nil))
    (pjones:urgency-hint (selected-frame) t)
    (circe-notifications-notify nick body channel)))

(defun pjones:circe-chat-mode-hook ()
  "Configure circe-mode."
  (tracking-mode)

  (let ((map circe-chat-mode-map))
    (define-key map (kbd "C-c C-q") 'circe-command-QUERY)))

(defun pjones:lui-mode-hook ()
  "Configure lui-mode."
  (auto-fill-mode -1)
  (enable-circe-color-nicks)
  (enable-lui-track-bar)

  (setq fringes-outside-margins t
        right-margin-width 5
        truncate-lines nil
        word-wrap t
        wrap-prefix "    ")

  ;; Simple mode-line for Circe buffers:
  (setq mode-line-format
        '("  %b " global-mode-string
                  tracking-mode-line-buffers))

  ;; Disable fringe indicators:
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil))


(add-hook 'circe-chat-mode-hook 'pjones:circe-chat-mode-hook)
(add-hook 'lui-mode-hook 'pjones:lui-mode-hook)
(add-hook 'circe-server-connected-hook 'enable-circe-notifications)

;###############################################################################
;;; Stolen code below.....

(defadvice circe-command-SAY (after jjf-circe-unignore-target)
  (let ((ignored (tracking-ignored-p (current-buffer) nil)))
    (when ignored
      (setq tracking-ignored-buffers
            (remove ignored tracking-ignored-buffers))
      (message "This buffer will now be tracked."))))

(ad-activate 'circe-command-SAY)

;; Taken from: https://github.com/jorgenschaefer/circe/wiki/Configuration
(defun circe-network-connected-p (network)
  "Return non-nil if there's any Circe server-buffer whose
`circe-server-netwok' is NETWORK."
  (catch 'return
    (dolist (buffer (circe-server-buffers))
      (with-current-buffer buffer
        (if (string= network circe-network)
            (throw 'return t))))))

;; Taken from: https://github.com/jorgenschaefer/circe/wiki/Configuration
(defun circe-maybe-connect (network)
  "Connect to NETWORK, but ask user for confirmation if it's
already been connected to."
  (interactive "sNetwork: ")
  (if (or (not (circe-network-connected-p network))
          (y-or-n-p (format "Already connected to %s, reconnect?" network)))
      (circe network)))

(provide 'circe-conf)
;;; circe-conf.el ends here
