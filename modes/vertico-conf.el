;;; vertico-conf.el -- Settings for `vertico' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'vertico)
(require 'vertico-multiform)

(declare-function embark-act "embark")
(declare-function embark-become "embark")
(declare-function embark-collect-completions "embark")
(declare-function marginalia-mode "marginalia")
(declare-function vertico-directory-delete-word "vertico-directory")
(declare-function vertico-prescient-mode "vertico-prescient")
(defvar crm-separator)

;; Prompt indicator for `completing-read-multiple'.
(when (< emacs-major-version 31)
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (string-replace "[ \t]*" "" crm-separator)
                              (car args))
                      (cdr args)))))

(let ((map vertico-map))
  (define-key map (kbd "C-<return>") #'embark-act)
  (define-key map (kbd "C-<tab>") #'embark-collect-completions)
  (define-key map (kbd "C-`") #'embark-become)
  (define-key map (kbd "C-w") #'vertico-directory-delete-word)
  (define-key map (kbd "M-F") #'vertico-multiform-flat)
  (define-key map (kbd "M-G") #'vertico-multiform-grid)
  (define-key map (kbd "M-U") #'vertico-multiform-unobtrusive)
  (define-key map (kbd "M-V") #'vertico-multiform-vertical))

(marginalia-mode)
(vertico-multiform-mode)
(vertico-prescient-mode)

;;; vertico-conf.el ends here
