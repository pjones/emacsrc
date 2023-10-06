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

(custom-set-variables
 '(vertico-multiform-commands
   '((consult-org-heading buffer)
     (consult-imenu buffer)
     (consult-buffer buffer
                     (vertico-buffer-display-action
                      . (display-buffer-in-direction
                         (direction . below)
                         (window-height . 0.3))))))
 '(vertico-multiform-categories
   '((consult-grep buffer))))

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

;;; vertico-conf.el ends here
