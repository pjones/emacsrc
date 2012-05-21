(defun pmade-server-start ()
  (when window-system
    (add-hook 'server-visit-hook 'save-place-find-file-hook)
    (server-start)))

(setq server-use-tcp t
      server-host (cond
                   ((string= "hawkins" system-name) "10.0.1.10")
                   ((string= "seward"  system-name) "192.168.218.1")
                   (t system-name)))

(add-hook 'after-init-hook 'pmade-server-start)
