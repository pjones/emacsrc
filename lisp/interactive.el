;;; interactive.el -- Interactive functions.
(eval-when-compile
  (require 'cl)                         ; for plusp (need to replace it)
  (require 'ispell))

(defun pjones:maybe-save-buffers-kill-terminal (&optional arg)
  "Save me from myself.  I somehow keep hitting C-x C-c when I
don't want to."
  (interactive)
  (if (yes-or-no-p "Really close this terminal? ")
      (save-buffers-kill-terminal arg)))

(defun pjones:kill-region-or-backward-kill-word (arg)
  "Replacement for `kill-region'.  If there is a region with
`transient-mark-mode' active, it will be removed and placed in
the kill ring in a similar manner to `kill-region'.  If there
isn't a region, the word before point will be deleted (without
placing it in the kill ring)."
  (interactive "p")
  (if (or (not transient-mark-mode) (and transient-mark-mode mark-active))
      (kill-region (region-beginning) (region-end))
    (delete-region (point) (progn (forward-word (- arg)) (point)))))

(defun pjones:switch-to-previous-buffer ()
  "Switch back to the last buffer shown in this window."
  (interactive)
  (switch-to-buffer (other-buffer nil t)))

(defun pjones:open-line-above ()
  "Open a line above point and move there."
  (interactive)
  (move-beginning-of-line nil)
  (open-line 1)
  (indent-according-to-mode))

(defun pjones:open-line-below ()
  "Open a line below the point, and move there"
  (interactive)
  (move-end-of-line 1)
  (newline)
  (indent-according-to-mode))

(defun pjones:irc (&optional local-only)
  "Start IRC client.  With an argument only start a connection to
the local bitlbee instance."
  (interactive "P")
  (require 'rcirc)
  (if local-only (rcirc-connect "localhost")
    (rcirc nil)
    (when (= 1 (length (window-list)))
      (split-window-right)
      (split-window-below)
      (split-window-below)
      (windmove-right)
      (split-window-below)
      (split-window-below)
      (balance-windows))))

(defun pjones:pwgen (&optional kill-only)
  "Generate and insert a password."
  (interactive "P")
  (let ((pw (replace-regexp-in-string "\n" ""
              (shell-command-to-string "pwgen -cnB 12 1"))))
    (kill-new pw)
    (if (not kill-only) (insert pw))))

(defun pjones:transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun pjones:toggle-dictionary ()
  (interactive)
  (let ((dict ispell-current-dictionary))
    (ispell-change-dictionary
     (if (string= dict "italian") "english" "italian"))
    (activate-input-method
     (if (string= dict "italian") nil "italian-postfix"))))

(defun pjones:insert-italian-name ()
  "Helper function for when I'm writing Italian dialog."
  (interactive)
  (let* ((names '("Alessandra" "Piera" "Carlotta" "Stefano"
                  "Valentina" "Paolo"))
         (pick (ido-completing-read "Name: " names)))
    (insert (concat "*" pick ":* "))))
