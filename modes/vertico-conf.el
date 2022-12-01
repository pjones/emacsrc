;;; vertico-conf.el -- Settings for `vertico' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'vertico)

(declare-function embark-act "embark")
(declare-function embark-become "embark")
(declare-function embark-collect-completions "embark")

(let ((map vertico-map))
  (define-key map (kbd "C-<tab>") #'embark-collect-completions)
  (define-key map (kbd "C-`") #'embark-become)
  (define-key map (kbd "C-<return>") #'embark-act))

;;; vertico-conf.el ends here
