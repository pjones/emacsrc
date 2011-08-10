;;; Local settings based on host name

(defun pmade-local-settings ()
  (cond
   ((string= "skinny.local" system-name) (pmade-local:skinny))
   ((string= "beefy.local"  system-name) (pmade-local:beefy))))

(defun pmade-local:skinny ()
  "Settings for my MacBook Pro"
  (set-frame-position (selected-frame) 674 22)
  (set-frame-size (selected-frame) 106 56))

(defun pmade-local:beefy ()
  "Settings for my Mac Pro")

;; These settings only take affect if we're running the GUI.
(if window-system
    (add-hook 'window-setup-hook 'pmade-local-settings t))
