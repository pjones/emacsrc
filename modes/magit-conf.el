;;; magit-conf.el -- Customizations for magit.
(eval-when-compile
  (require 'magit))

;; Shamelessly stolen from http://whattheemacsd.com/.
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun pjones:magit-quit-session ()
  "Restore the previous window configuration and kill the magit
buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'pjones:magit-quit-session)
