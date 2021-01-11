;;; icomplete-conf.el -- Settings for `icomplete' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'evil)
(require 'icomplete)

(autoload 'embark-collect-completions "embark")

(custom-set-variables
  '(icomplete-separator " · ")
  '(icomplete-show-matches-on-no-input nil)
  '(icomplete-prospects-height 1)
  '(completion-styles '(orderless)))

(defun pjones:icomplete-tab ()
  "Show `embark-collect-completions' or tab complete."
  (interactive)
  (let ((buffer (get-buffer "*Embark Collect Completions*")))
    (if buffer (icomplete-forward-completions)
      (embark-collect-completions))))

(defun pjones:icomplete-slash ()
  "Interpret the slash character."
  (interactive)
  (if (eq (icomplete--category) 'file)
      (cond
       ((or (eq (char-before) ?~)
            (eq (char-before) ?/))
        (insert "/")
        (minibuffer-complete))
       (t
        (icomplete-fido-ret)))
    (insert "/")))

(evil-define-key 'insert icomplete-minibuffer-map
  (kbd "/") #'pjones:icomplete-slash
  (kbd "<right>") #'icomplete-forward-completions
  (kbd "<left>") #'icomplete-backward-completions
  (kbd "<tab>") #'pjones:icomplete-tab
  (kbd "<backspace>") #'icomplete-fido-backward-updir
  (kbd "C-<escape>") #'embark-become
  (kbd "C-j") #'exit-minibuffer ; Accept input w/o completion.
  (kbd "RET") #'icomplete-fido-ret
  (kbd "C-<return>") #'embark-act)

;;; icomplete-conf.el ends here
