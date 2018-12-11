;;; suspend.el --- Secure Emacs on a system suspend, sleep, lock, etc.
;;
;;; Commentary:
;;
;;; Code:
;;
;; Require libraries at run time.
(require 'server)

;; Call this function to tell Emacs that the computer is about to
;; suspend or lock.
(defun pjones:suspend ()
  "Prepare to suspend."
  (interactive)
  (pjones:close-encrypted-buffers))

(defun pjones:close-encrypted-buffers ()
  "Close all buffers showing encrypted content."
  (require 'epa-hook)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name
                 (string-match epa-file-name-regexp buffer-file-name))
        (when (buffer-modified-p buf) (basic-save-buffer))
        (kill-buffer buf)))))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; suspend.el ends here
