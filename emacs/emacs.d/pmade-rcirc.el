(require 'rcirc)
(eval-after-load 'rcirc (require 'rcirc-color))

;; Load passwords from a private file
(setq pmade-comm-sync "~/.comm-sync")
(load (concat pmade-comm-sync "/etc/erc/erc.el"))

(setq rcirc-default-nick "pmade"
      rcirc-default-user-name "pmade"
      rcirc-default-full-name "Peter Jones")

(setq 
 rcirc-server-alist
 '(("localhost")
   ("irc.freenode.net" :channels ("#org-mode" "#latex" "#photogeeks" "#emacs"))))

(setq rcirc-authinfo
      `(("freenode"  nickserv "pmade" ,freenode-password)
        ("localhost" bitlbee  "pmade" ,bitlbee-password)))

(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
      rcirc-buffer-maximum-lines 500)

(defun-rcirc-command all (input)
  "Run the arguments as a command for all connections.
Example use: /all away food or /all quit zzzz."
  (interactive "s")
  (let ((buffers (mapcar 'process-buffer (rcirc-process-list))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (goto-char (point-max))
        (insert "/" input)
        (rcirc-send-input)))))

(defun pmade:rcirc-macrumors ()
  "Connect to the macrumors IRC server."
  (rcirc-connect "irc.macrumorslive.com" nil nil nil nil "#macrumors"))

(defun pmade:rcirc-hook ()
  (unless (string-match "localhost" (buffer-name))
    (setq rcirc-ignore-buffer-activity-flag t)
    (rcirc-omit-mode))
  (define-key rcirc-mode-map (kbd "C-c C-o") 'rcirc-browse-url)
  (set (make-local-variable 'scroll-conservatively) 8192)
  (flyspell-mode t)
  (rcirc-track-minor-mode 1))

(defun pmade:rcirc-activity-string ()
  (when (string= "[]" rcirc-activity-string)
    (setq rcirc-activity-string "")))

(defun pmade:rcirc-notify (sender text)
  "Display a notification using growlnotify."
  (call-process "/opt/local/bin/growlnotify"
                nil nil nil "-n" "Chat"
                "-a" "iChat" "-d" (concat "rcirc-" sender)
                "-m" text (concat "Message from " sender)))

(defun pmade:rcirc-print-hook (proc sender response target text)
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
                    (not (string= (rcirc-server-name proc) sender))))
           (not (get-buffer-window buf)))
      (pmade:rcirc-notify sender text))))
  
(add-hook 'rcirc-mode-hook 'pmade:rcirc-hook)
(add-hook 'rcirc-update-activity-string-hook 'pmade:rcirc-activity-string)
(add-hook 'rcirc-print-hooks 'pmade:rcirc-print-hook)
