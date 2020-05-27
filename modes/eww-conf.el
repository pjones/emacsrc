;;; eww-conf.el -- Settings for EWW.
;;
;;; Commentary:
;;
;;; Code:
(require 'eww)

(custom-set-variables
 '(eww-download-directory "~/download"))

(evil-leader/set-key-for-mode 'eww-mode
  "m r" #'pjones:eww-rename-buffer)

(defun pjones:eww-rename-buffer ()
  "Rename the current `eww' buffer to include its title."
  (interactive)
  (let ((name (concat "*eww: " (plist-get eww-data :title) "*")))
    (rename-buffer name t)))

;;; eww-conf.el ends here
