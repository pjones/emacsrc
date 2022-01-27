;;; puni-conf.el -- Settings for `puni' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'puni)

(defun pjones:puni-squeeze-splice (arg)
  "Perform a squeeze.
If ARG is non-nil then perform a splice."
  (interactive "P")
  (if arg (puni-splice)
    (puni-squeeze)))

(let ((map puni-mode-map))
  ;; I can't deal with my editor preventing me from deleting
  ;; delimiters:
  (define-key map (kbd "DEL") nil)
  (define-key map (kbd "C-d") nil)
  (define-key map (kbd "C-w") nil)
  (define-key map (kbd "C-M-/") #'puni-split)
  (define-key map (kbd "C-M-k") #'pjones:puni-squeeze-splice)
  (define-key map (kbd "C-M-t") #'puni-transpose))

;;; puni-conf.el ends here
