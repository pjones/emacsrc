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

;; Each server should get its own treemacs config:
(defvar treemacs-persist-file)
(defvar treemacs-last-error-persist-file)
(defun pjones:set-treemacs-persist-file ()
  "Set `treemacs-persist-file' so servers don't clobber it."
  (setq treemacs-persist-file
        (concat
         user-emacs-directory
         "treemacs/"
         server-name)
        treemacs-last-error-persist-file
        (concat treemacs-persist-file ".error")))
(add-hook
 'pjones:after-server-hook
 #'pjones:set-treemacs-persist-file)

;;; server.el ends here
