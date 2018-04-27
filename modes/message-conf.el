;;; message-conf.el --- composing mail and news messages
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'company)
  (require 'message))

(defun pjones:message-mode-hook ()
  "Configure message mode to my liking."
  ;; Configure completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-bbdb)
  (add-to-list 'company-backends 'company-ispell))

(add-hook 'message-mode-hook 'pjones:message-mode-hook)

(provide 'message-conf)
;;; message-conf.el ends here
