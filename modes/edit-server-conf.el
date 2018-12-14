;;; edit-server-conf.el --- Settings for edit-server
;;
;;; Commentary:
;;
;;; Code:
(require 'edit-server)

(custom-set-variables
 '(edit-server-new-frame nil)
 '(edit-server-default-major-mode 'markdown-mode))

;; Fix a really buggy issue with edit-server:
(define-key edit-server-edit-mode-map (kbd "C-x C-s") #'save-buffer)

;;; edit-server-conf.el ends here
