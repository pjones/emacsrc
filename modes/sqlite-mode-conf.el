;;; sqlite-mode-conf.el -- Settings for `sqlite-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'sqlite-mode)
(require 'sqlite-mode-extras)

;; Adapted from:
;;
;; https://www.forum.christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/
(defun pjones:sqlite-mode-open-file ()
  "Handle sqlite3 files automatically.
Runs `sqlite-mode-open-file' on the file name visited by the current
buffer, killing it."
  (let ((file-name buffer-file-name))
    (kill-current-buffer)
    (sqlite-mode-open-file file-name)))

(add-hook 'sqlite-mode-hook #'sqlite-extras-minor-mode)

;;; sqlite-mode-conf.el ends here
