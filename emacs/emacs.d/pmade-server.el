(defun pmade-server-start ()
  (when window-system
    (add-hook 'server-visit-hook 'save-place-find-file-hook)
    (server-start)))

(setq server-use-tcp t
      server-host (if (string= "beefy.local" system-name) 
                      "192.168.31.1" system-name))

(add-hook 'after-init-hook 'pmade-server-start)
