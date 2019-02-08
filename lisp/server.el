;;; server.el -- Add some features to Emacs' server/daemon.
;;
;;; Commentary:
;;
;;; Code:

(require 'server)

(defvar pjones:after-server-hook nil
  "Hook run after Emacs server starts.")

;; Get notified when the server starts.
(defun pjones:after-server-start (&rest _)
  "Called after `server-start'."
  (run-hooks 'pjones:after-server-hook))

(advice-add #'server-start :after #'pjones:after-server-start)

;;; server.el ends here
