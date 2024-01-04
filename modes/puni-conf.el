;;; puni-conf.el -- Settings for `puni' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'puni)

(let ((map puni-mode-map))
  ;; I can't deal with my editor preventing me from deleting
  ;; delimiters:
  (define-key map (kbd "DEL") nil)
  (define-key map (kbd "C-d") nil)
  (define-key map (kbd "C-k") nil)
  (define-key map (kbd "C-w") nil)
  (define-key map (kbd "C-M-k .") #'kill-sexp)
  (define-key map (kbd "C-M-k b") #'puni-slurp-forward)
  (define-key map (kbd "C-M-k f") #'puni-barf-forward)
  (define-key map (kbd "C-M-k k") #'puni-squeeze)
  (define-key map (kbd "C-M-k RET") #'puni-split)
  (define-key map (kbd "C-M-k s") #'puni-splice)
  (define-key map (kbd "C-M-k t") #'puni-transpose))

;;; puni-conf.el ends here
