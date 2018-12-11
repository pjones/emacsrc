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
  (let ((win (selected-window))
        (buf (current-buffer)))
    (pdf-outline-follow-link nil)
    (delete-window win)
    (kill-buffer buf)))

(defun pjones:pdf-outline-quit ()
  "Close the outline buffer and window."
  (interactive)
  (let ((win (selected-window))
        (buf (current-buffer)))
    (delete-window win)
    (kill-buffer buf)))

(defun pjones:pdf-view-after-change-page-hook ()
  "Respond to a page change in PDF documents."
  ;; Make sure the cursor is turned off.  (God mode turns it back on
  ;; from time to time.)
  (setq cursor-type nil))

;; Settings:
(custom-set-variables
 '(pdf-view-continuous nil))

;; Extra key bindings:
(define-key pdf-outline-buffer-mode-map (kbd "RET") #'pjones:pdf-outline-follow-link-and-quit)
(define-key pdf-outline-buffer-mode-map (kbd "q")   #'pjones:pdf-outline-quit)

;; Hooks:
(add-hook 'pdf-view-after-change-page-hook #'pjones:pdf-view-after-change-page-hook)

;;; pdf-tools-conf.el ends here