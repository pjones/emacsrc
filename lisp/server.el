;;; server.el -- Add some features to Emacs' server/daemon.
;;
;;; Commentary:
;;
;;; Code:

(require 'server)

(declare-function pjones:load-theme "./theme")

(defvar pjones:after-server-hook nil
  "Hook run after Emacs server starts.")

;; Get notified when the server starts.
(defun pjones:after-server-start (&rest _)
  "Called after `server-start'."
  (run-hooks 'pjones:after-server-hook))
(advice-add #'server-start :after #'pjones:after-server-start)

(defun pjones:notes-server-hook ()
  "Set up the notes server."
  (when (string= server-name "notes")
    (auto-save-visited-mode 1)
    (pjones:load-theme 'doom-snazzy)))

(defun pjones:mail-server-hook ()
  "Set up the mail server."
  (when (string= server-name "mail")
    (pjones:load-theme 'doom-molokai)))

(add-hook 'pjones:after-server-hook #'pjones:mail-server-hook)
(add-hook 'pjones:after-server-hook #'pjones:notes-server-hook)

;;; server.el ends here
