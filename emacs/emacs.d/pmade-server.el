(defun pmade-server-start ()
  (when window-system
    (add-hook 'server-visit-hook 'save-place-find-file-hook)
    (server-start)))

(setq server-use-tcp t
      server-host (if (string= "hawkins" system-name)
                      "10.0.1.10" system-name))

(add-hook 'after-init-hook 'pmade-server-start)
