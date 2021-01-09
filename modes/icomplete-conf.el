;;; icomplete-conf.el -- Settings for `icomplete' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'evil)
(require 'icomplete)

(custom-set-variables
  '(icomplete-separator " · ")
  '(icomplete-prospects-height 1)
  '(completion-styles '(orderless)))

(evil-define-key 'insert icomplete-minibuffer-map
  (kbd "<right>") #'icomplete-forward-completions
  (kbd "<left>") #'icomplete-backward-completions
  (kbd "<tab>") #'embark-collect-completions
  (kbd "<backspace>") #'icomplete-fido-backward-updir
  (kbd "C-<escape>") #'embark-become
  (kbd "C-j") #'exit-minibuffer ; Accept input w/o completion.
  (kbd "RET") #'icomplete-fido-ret
  (kbd "C-<return>") #'embark-act)

;;; icomplete-conf.el ends here
