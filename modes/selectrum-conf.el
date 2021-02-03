;;; selectrum-conf.el -- Settings for `selectrum' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'evil)
(require 'selectrum)

(require 'selectrum-prescient)
(selectrum-prescient-mode)

(declare-function marginalia-mode "marginalia")

(custom-set-variables
 '(selectrum-display-style '(horizontal))
 '(marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil)))

(defun pjones:selectrum-slash ()
  "Interpret the slash character."
  (interactive)
  (if minibuffer-completing-file-name
      (if (or (eq (char-before) ?~)
              (eq (char-before) ?:)
              (looking-back "[[:alnum:]]+:/" 3))
          (insert "/")
        (selectrum-insert-current-candidate))
    (insert "/")))

(defun pjones:selectrum-delete ()
  "Move upward in the file hierarchy."
  (interactive)
  (if (and minibuffer-completing-file-name
           (eq (char-before) ?/))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively #'backward-delete-char)))

(defun pjones:selectrum-toggle-marginalia ()
  "Toggle `marginalia-mode' based on `selectrum-display-style'."
  (if (eq selectrum-display-style 'horizontal)
      (marginalia-mode -1)
    (marginalia-mode +1)))
(advice-add
 'selectrum-cycle-display-style
 :after #'pjones:selectrum-toggle-marginalia)

(evil-define-key 'insert selectrum-minibuffer-map
  (kbd "/") #'pjones:selectrum-slash
  (kbd "<backspace>") #'pjones:selectrum-delete
  (kbd "<right>") #'selectrum-previous-candidate
  (kbd "<left>") #'selectrum-next-candidate
  (kbd "<tab>") #'selectrum-next-candidate
  (kbd "C-<tab>") #'embark-collect-completions
  (kbd "C-<escape>") #'embark-become
  (kbd "C-<return>") #'embark-act
  (kbd "M-a") #'marginalia-cycle)

;;; selectrum-conf.el ends here
