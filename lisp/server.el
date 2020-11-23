;;; server.el -- Add some features to Emacs' server/daemon.
;;
;;; Commentary:
;;
;;; Code:

(require 'server)
(require 'cl-lib)

(declare-function pjones:load-theme "./theme")

(defvar pjones:after-server-hook nil
  "Hook run after Emacs server starts.")

;; Get notified when the server starts.
(defun pjones:after-server-start (&rest _)
  "Called after `server-start'."
  (run-hooks 'pjones:after-server-hook))
(advice-add #'server-start :after #'pjones:after-server-start)

;; Make server.el STFU.
(defun pjones:server-execute-stfu (orig &rest args)
  "Prevent `server-execute' (ORIG) from using the `message' command.
ARGS are passed on to `server-execute'."
  (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
    (apply orig args))
  ;; And then clear the echo area so modes like vterm don't start out
  ;; with a smaller than expected buffer height:
  (message "%s" ""))
(advice-add #'server-execute :around #'pjones:server-execute-stfu)

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

(defvar desktop-base-file-name)
(defvar desktop-base-lock-name)
(defun pjones:desktop-server-vars ()
  "Set per-server variables for `desktop-mode'."
  (setq desktop-base-file-name (concat server-name ".desktop")
        desktop-base-lock-name (concat server-name ".desktop.lock")))
(add-hook
 'pjones:after-server-hook
 #'pjones:desktop-server-vars)

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
