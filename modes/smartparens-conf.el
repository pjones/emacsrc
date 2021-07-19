;;; smartparens-conf.el -- Settings for `smartparens' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'smartparens)
(require 'smartparens-config)

;; Use default key bindings:
(sp-use-smartparens-bindings)

(let ((mode smartparens-mode-map))
  (define-key mode (kbd "C-M-0") #'sp-unwrap-sexp)
  (define-key mode (kbd "C-M-5") #'sp-forward-barf-sexp)
  (define-key mode (kbd "C-M-6") #'sp-forward-slurp-sexp)
  (define-key mode (kbd "C-M-9") #'sp-rewrap-sexp)
  (define-key mode (kbd "C-M-SPC") nil))

;;; smartparens-conf.el ends here
