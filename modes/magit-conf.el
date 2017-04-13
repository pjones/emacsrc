;;; magit-conf.el -- Customizations for magit.
(eval-when-compile
  (defvar magit-last-seen-setup-instructions)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (require 'magit))

(custom-set-variables
 '(magit-popup-use-prefix-argument 'default)
 '(magit-status-margin '(t age magit-log-margin-width nil 18))
 '(magit-status-show-hashes-in-headers t))

(defun pjones:magit-quit ()
  "Quit magit and close the frame."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (eq major-mode 'magit-status-mode)
      (delete-frame nil t))
    (with-current-buffer buffer
      (magit-mode-bury-buffer))))

(defun pjones:magit-mode-hook ()
  (let ((map magit-mode-map))
    (define-key map (kbd "q") 'pjones:magit-quit)))

(add-hook 'magit-mode-hook 'pjones:magit-mode-hook)
