;;; csv-mode-conf.el -- Settings for `csv-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'csv-mode)

(defun pjones:csv-mode-hook ()
  "Hook run by `csv-mode'."
  (let ((map csv-mode-map))
    (define-key map (kbd "C-M-b") #'csv-backward-field)
    (define-key map (kbd "C-M-f") #'csv-forward-field)))

(add-hook 'csv-mode-hook #'pjones:csv-mode-hook)
(add-hook 'csv-mode-hook #'csv-align-mode)

;;; csv-mode-conf.el ends here
