;;; edit-server-conf.el --- Settings for edit-server
;;
;;; Commentary:
;;
;;; Code:
(require 'edit-server)
(require 'markdown-mode)

(declare-function pjones:markdown-visual-line "./markdown-mode-conf.el")

(custom-set-variables
 '(edit-server-new-frame nil)
 '(edit-server-default-major-mode 'markdown-mode))

;; Fix a really buggy issue with edit-server:
(define-key edit-server-edit-mode-map (kbd "C-x C-s") #'save-buffer)

;; Put markdown-mode into the right state:
(add-hook 'edit-server-edit-mode-hook #'pjones:markdown-visual-line)

;;; edit-server-conf.el ends here
