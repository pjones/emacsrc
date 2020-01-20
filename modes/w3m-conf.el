;;; w3m-conf.el --- Settings for w3m.
;;
;;; Commentary:
;;
;;; Code:
(require 'w3m)
(require 'evil-leader)

(custom-set-variables
 '(w3m-use-tab nil)
 '(w3m-pop-up-windows nil)
 '(w3m-pop-up-frames nil)
 '(w3m-display-mode 'plain)
 '(w3m-use-header-line t)
 '(w3m-use-header-line-title t)
 '(w3m-make-new-session t))

(evil-leader/set-key-for-mode 'w3m-mode
  "DEL r" #'pjones:w3m-rename-buffer)

(defun pjones:w3m-rename-buffer ()
  "Rename the current `w3m' buffer to include its title."
  (interactive)
  (let ((name (concat "*w3m: " w3m-current-title "*")))
    (rename-buffer name t)))

;;; w3m-conf.el ends here
