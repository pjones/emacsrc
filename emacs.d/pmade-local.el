;;; Local settings based on host name

(defun pmade-local-settings ()
  (let ((host (replace-regexp-in-string "\n" "" (shell-command-to-string "hostname"))))
    (cond
     ((string= "skinny.local" host) (pmade-local:skinny))
     ((string= "beefy.local"  host) (pmade-local:beefy)))))

(defun pmade-center-frame (frame)
  "Move the frame to the center of the screen.  The screen space
reported by the `display-pixel-height' function will be adjusted
to account for a missing Mac OS X menu bar."
  (let* ((display-w (display-pixel-width))
         (display-h (- (display-pixel-height) 20)) ; remove menu bar
         (frame-w (frame-pixel-width frame))
         (frame-h (frame-pixel-height frame))
         (left (- (/ display-w 2) (/ frame-w 2)))
         (top  (- (/ display-h 2) (/ frame-h 2))))
    (set-frame-position frame left top)))

(defun pmade-local:skinny ()
  "Settings for my MacBook Pro"
  (let ((frame (selected-frame)))
    (set-frame-size frame 171 54)
    (pmade-center-frame frame)))

(defun pmade-local:beefy ()
  "Settings for my Mac Pro"
  (let ((frame (selected-frame)))
    (set-frame-size frame 191 63)
    (pmade-center-frame frame)))

(defun pmade-local:gui-specific ()
  "Settings for the GUI Emacs."
  (dotimes (i 4) (elscreen-create))
  (elscreen-goto 0))

;; These settings only take affect if we're running the GUI.
(when window-system
  (add-hook 'window-setup-hook 'pmade-local-settings t)
  (add-hook 'window-setup-hook 'pmade-local:gui-specific t))

