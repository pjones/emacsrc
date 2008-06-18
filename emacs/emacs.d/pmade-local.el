;;; Local settings based on host name

(defun pmade-local-settings ()
  (let ((host (replace-regexp-in-string "\n" "" (shell-command-to-string "hostname"))))
    (cond
     ((string= "skinny.local" host) (pmade-local:skinny))
     ((string= "beefy.local"  host) (pmade-local:beefy)))))

(defun pmade-local:skinny ()
  "Settings for my MacBook Pro"
  (let ((frame (selected-frame)))
    (set-frame-position frame 106 6)
    (set-frame-size frame 171 54)))

(defun pmade-local:beefy ()
  "Settings for my MacBook Pro"
  (let ((frame (selected-frame)))
    (set-frame-position frame 116 6)
    (set-frame-size frame 191 63)))

(add-hook 'window-setup-hook 'pmade-local-settings t)
