;;; pdf-tools-conf.el -- Setting for pdf-tools.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'pdf-tools))

;; Dependencies:
(require 'pdf-outline)

(defun pjones:pdf-outline-follow-link-and-quit ()
  "Follow outline link and delete the outline window."
  (interactive)
  (let ((win (selected-window)))
    (pdf-outline-follow-link nil)
    (delete-window win)))

;; Settings:
(custom-set-variables
 '(pdf-view-continuous nil))

;; Extra key bindings:
(define-key pdf-outline-buffer-mode-map (kbd "RET") #'pjones:pdf-outline-follow-link-and-quit)

;;; pdf-tools-conf.el ends here
