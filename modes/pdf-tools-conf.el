;;; pdf-tools-conf.el -- Setting for pdf-tools.
;;
;;; Commentary:
;;
;;; Code:
(require 'pdf-tools)
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
  (blink-cursor-mode -1)
  (setq-local cursor-type nil))

;; Settings:
(custom-set-variables
 '(pdf-view-continuous nil))

(defun pjones:pdf-view-mode-hook ()
  "Hook for `pdf-view-mode-hook'."
  (pdf-view-fit-page-to-window)
  (pdf-view-midnight-minor-mode)
  (pjones:pdf-view-after-change-page-hook))

(let ((map pdf-outline-buffer-mode-map))
  (define-key map (kbd "RET") #'pjones:pdf-outline-follow-link-and-quit)
  (define-key map (kbd "q") #'pjones:pdf-outline-quit))

;; Hooks:
(add-hook 'pdf-view-after-change-page-hook #'pjones:pdf-view-after-change-page-hook)
(add-hook 'pdf-view-mode-hook #'pjones:pdf-view-mode-hook)

;;; pdf-tools-conf.el ends here
