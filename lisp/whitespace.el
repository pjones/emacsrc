;;; whitespace.el -- Correctly set my load-path variable. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(defun pjones:delete-whitespace ()
  "Delete trailing whitespace."
  (delete-trailing-whitespace))

(define-minor-mode pjones:delete-whitespace-mode
  "Delete whitespace when saving a file."
  :init-value nil
  :lighter nil
  (if pjones:delete-whitespace-mode
      (add-hook 'before-save-hook #'pjones:delete-whitespace nil t)
    (remove-hook 'before-save-hook #'pjones:delete-whitespace t)))

;;; whitespace.el ends here
