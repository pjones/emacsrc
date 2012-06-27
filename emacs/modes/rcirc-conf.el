;;; rcirc-conf.el -- Settings for rcirc.
(eval-when-compile
  (load "../lisp/functions.el")
  (require 'rcirc))

;; Silence compiler warnings
(declare-function rcirc-omit-mode "rcirc")
(declare-function rcirc-track-minor-mode "rcirc")
(declare-function rcirc-nick "rcirc")
(declare-function rcirc-server-name "rcirc")

(defvar freenode-password nil
  "My nick password for irc.freenode.net.")

(defvar bitlbee-password nil
  "My account password for bitlbee.")

;; Load passwords from a private file
(load "~/keys/emacs/secrets.el")

(setq rcirc-default-nick "pmade"
      rcirc-default-user-name "pmade"
      rcirc-default-full-name "Peter Jones"
      rcirc-fill-column 70)

(setq rcirc-server-alist
      '(("localhost" :nick "pjones")
        ("irc.freenode.net" :channels ("#xmonad" "#derailed" "#mpd"
                                       "#conkeror" "#debian" "#emacs"))))

(setq rcirc-authinfo
      `(("freenode"  nickserv "pmade"  ,freenode-password)
        ("localhost" bitlbee  "pjones" ,bitlbee-password)))

(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
      rcirc-buffer-maximum-lines 500)

(defun pjones:rcirc-cmd-all (input)
  "See the docs for rcirc-cmd-all."
  (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "/" input)
        (rcirc-send-input)))))

(defun-rcirc-command all (input)
  "Run the arguments as a command for all connections.
Example use: /all away food or /all quit zzzz."
  (interactive "s")
  (pjones:rcirc-cmd-all input))

(defun pjones:rcirc-quit ()
  "Quit all rcirc connections."
  (interactive)
  (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (rcirc-cmd-quit "bye.")))))

(defun pjones:rcirc-macrumors ()
  "Connect to the macrumors IRC server."
  (interactive)
  (rcirc-connect "irc.macrumorslive.com" nil nil nil nil "#macrumors"))

(defun pjones:rcirc-hook ()
  (require 'rcirc-color)
  (when (and (string-match "#" (buffer-name))
             (not (string-match "developers\\|derailed\\|twitter" (buffer-name))))
    (setq rcirc-ignore-buffer-activity-flag t)
    (rcirc-omit-mode))
  (define-key rcirc-mode-map (kbd "C-c C-o") 'rcirc-browse-url)
  (set (make-local-variable 'scroll-conservatively) 8192)
  (set (make-local-variable 'next-line-add-newlines) nil)
  (setq mode-line-format '("  %b " global-mode-string))
  (flyspell-mode t)
  (rcirc-track-minor-mode 1))

(defun pjones:rcirc-activity-string ()
  (when (string= "[]" rcirc-activity-string)
    (setq rcirc-activity-string "")))

(defun pjones:rcirc-notify (sender text)
  "Display an activity notification."
  (pjones:urgency-hint (selected-frame) t))

(defun pjones:rcirc-print-hook (proc sender response target text)
  "Hook called when a new IRC message is received.  If the
message is either a PRIVMSG or mentions your nick, use the
notification function.  This only happens if the target buffer is
not currently displayed in a window."
  (let ((buf (rcirc-get-buffer proc target)))
    (when (and
           (or (and (string= response "PRIVMSG")
                    (not (string= (rcirc-nick proc) sender))
                    (not (rcirc-channel-p target)))
               (and (string-match (rcirc-nick proc) text)
                    (not (string= (rcirc-nick proc) sender))
                    (not (string= (rcirc-server-name proc) sender)))))
;           (not (get-buffer-window buf)))
      (pjones:rcirc-notify sender text))))

(add-hook 'rcirc-mode-hook 'pjones:rcirc-hook)
(add-hook 'rcirc-update-activity-string-hook 'pjones:rcirc-activity-string)
(add-hook 'rcirc-print-hooks 'pjones:rcirc-print-hook)
