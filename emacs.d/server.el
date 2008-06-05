;; If this is a GUI emacs, or running from inside a screen session,
;; start the server for emacsclient
(setq screen-session-name (getenv "SCREEN_SESSION_NAME"))

(defun pmade-server-start ()
  (when (or window-system screen-session-name)
    (and screen-session-name (setq server-name screen-session-name))
    (add-hook 'server-visit-hook 'save-place-find-file-hook)
    (server-start)))

(add-hook 'after-init-hook 'pmade-server-start)
