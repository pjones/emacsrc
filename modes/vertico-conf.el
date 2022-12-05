;;; vertico-conf.el -- Settings for `vertico' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-posframe)

(declare-function embark-act "embark")
(declare-function embark-become "embark")
(declare-function embark-collect-completions "embark")
(declare-function vertico-directory-delete-word "vertico-directory")

(custom-set-variables
 '(vertico-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner)
 '(vertico-multiform-commands
   '((execute-extended-command unobtrusive)
     (projectile-find-file posframe grid)
     (project-switch-project posframe grid)
     (consult-org-heading buffer ,(lambda (_) (text-scale-set -1)))
     (consult-imenu buffer)
     (consult-buffer buffer
                     (vertico-buffer-display-action
                      . (display-buffer-in-direction
                         (direction . below)
                         (window-height . 0.1))))))
 '(vertico-multiform-categories
   '((file posframe grid)
     (bookmark posframe grid)
     (symbol-help posframe)
     (consult-grep buffer))))

(let ((map vertico-map))
  (define-key map (kbd "C-<return>") #'embark-act)
  (define-key map (kbd "C-<tab>") #'embark-collect-completions)
  (define-key map (kbd "C-`") #'embark-become)
  (define-key map (kbd "C-w") #'vertico-directory-delete-word)
  (define-key map (kbd "M-F") #'vertico-multiform-flat)
  (define-key map (kbd "M-G") #'vertico-multiform-grid)
  (define-key map (kbd "M-U") #'vertico-multiform-unobtrusive)
  (define-key map (kbd "M-V") #'vertico-multiform-vertical))

(vertico-multiform-mode)

;;; vertico-conf.el ends here
