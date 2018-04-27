;;; suspend.el --- Secure Emacs on a system suspend, sleep, lock, etc.
(eval-when-compile
  (require 'circe))

;; Require libraries at run time.
(require 'server)

;; Call this function to tell Emacs that the computer is about to
;; suspend or lock.
(defun pjones:suspend ()
  "Prepare to suspend."
  (interactive)
  (pjones:close-encrypted-buffers)
  (cond
   ((string= server-name "irc")
    (pjones:circe-quit))
   ((string= server-name "gnus")
    (pjones:gnus-quit))))

(defun pjones:close-encrypted-buffers ()
  "Close all buffers showing encrypted content."
  (require 'epa-hook)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-match epa-file-name-regexp buffer-file-name))
        (when (buffer-modified-p buf) (basic-save-buffer))
        (kill-buffer buf)))))

;; Once bug #282 is fixed this can become a simple wrapper around
;; `circe-command-GQUIT': https://github.com/jorgenschaefer/circe/issues/282
(defun pjones:circe-quit ()
  "Disconnect from all servers."
  (interactive)
  (require 'circe)
  (dolist (buf (circe-server-buffers))
    (with-current-buffer buf
      (circe-command-QUIT circe-default-quit-message))))

(defun pjones:gnus-quit ()
  "Disconnect from all mail servers."
  (require 'gnus)
  (let ((gnus-expert-user t)
        (gnus-interactive-exit 'quiet))
    (gnus-group-exit)))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
