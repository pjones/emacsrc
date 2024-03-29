;;; erc-conf.el -- Settings for erc.el
;;
;;; Commentary:
;;
;;; Code:
;; Dependencies:

(require 'adaptive-wrap)
(require 'erc)
(require 'erc-track)
(require 'notifications)
(require 'passmm)

(declare-function passmm-get-password "passmm")
(declare-function pjones:urgency-hint "../lisp/functions.el")

(custom-set-variables
 '(erc-nick "pmade")
 '(erc-user-full-name "Peter Jones")
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
 '(erc-track-position-in-mode-line t)
 '(erc-timestamp-format "[%H:%M] ")
 '(erc-timestamp-format-left "[%H:%M] ")
 '(erc-insert-timestamp-function 'erc-insert-timestamp-left)
 '(erc-insert-away-timestamp-function 'erc-insert-timestamp-left)
 '(erc-server-auto-reconnect nil)
 '(erc-timestamp-only-if-changed-flag nil)

 '(erc-modules '(autojoin
                 button
                 completion
                 hl-nicks
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


(custom-set-faces
 '(erc-timestamp-face ((t (:foreground nil :inherit 'mode-line-inactive))))
 '(erc-input-face ((t (:foreground nil :inherit 'erc-fool-face))))
 '(erc-my-nick-face ((t (:foreground nil :inherit 'erc-fool-face)))))

;; Always ignore the bitlbee control channel.
(add-to-list 'erc-track-exclude "&bitlbee")

(defvar pjones:erc-modified-channels-alist nil
  "A cache of `erc-modified-channels-alist'.")

(defun pjones:erc-set-urgency-hint (buffer)
  "Set the urgency hint on BUFFER.
Really, it needs to be set on a frame, so search for the correct frame
if BUFFER is not currently displayed in a window."
  (let ((found (car (frame-list)))
        frame window)
    (if (setq window (get-buffer-window buffer t))
        (pjones:urgency-hint (window-frame window) t)
      ;; Damn, go find it's most recent window:
      (catch 'pjones:find-frame
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (when (member buffer (mapcar 'car (window-prev-buffers window)))
              (setq found frame)
              (throw 'pjones:find-frame t)))))
      (pjones:urgency-hint found t))))

(defun pjones:erc-maybe-set-urgency-hint ()
  "Maybe set the urgency hint."
  (let (channel buffer)
    (dolist (channel erc-modified-channels-alist)
      (setq buffer (car channel))
      (unless (assoc buffer pjones:erc-modified-channels-alist)
        (pjones:erc-set-urgency-hint buffer))))
  (setq pjones:erc-modified-channels-alist
        erc-modified-channels-alist))

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
  (make-local-variable 'scroll-conservatively)
  (setq scroll-conservatively 1000      ; Don't recenter window
        adaptive-wrap-extra-indent 8)   ; Leave space for timestamp.
  (visual-line-mode)
  (adaptive-wrap-prefix-mode))

(defun pjones:erc-ignore-channel ()
  "Disable ERC tracking for channels."
  (interactive)
  (when (and (erc-default-target)
             (string-match-p "^#" (erc-default-target)))
    (add-to-list 'erc-track-exclude (erc-default-target))))

(add-function
 :after after-focus-change-function
 (lambda (&rest args) (erc-modified-channels-update)))

(add-hook 'erc-mode-hook #'pjones:erc-mode-hook)
(add-hook 'erc-track-mode-hook #'pjones:erc-ignore-channel)
(add-hook 'erc-track-list-changed-hook #'pjones:erc-maybe-set-urgency-hint)

;;; erc-conf.el ends here
