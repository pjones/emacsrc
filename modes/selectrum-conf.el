;;; selectrum-conf.el -- Settings for `selectrum' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'consult)
(require 'consult-selectrum)
(require 'embark)
(require 'marginalia)
(require 'selectrum)

(require 'selectrum-prescient)
(selectrum-prescient-mode)

(declare-function marginalia-mode "marginalia")

(custom-set-variables
 '(selectrum-display-style '(vertical horizontal))
 '(marginalia-annotators
   '(marginalia-annotators-heavy
     marginalia-annotators-light
     nil)))

(defun pjones:selectrum-slash ()
  "Interpret the slash character.

If it looks like the user wants to complete the current candidate then
entering a slash will have the same affect as pressing the tab key.

But, in situations where a literal slash is needed, insert one instead.

A literal slash can always be added by using \\[quoted-insert]."
  (interactive)
  (if minibuffer-completing-file-name
      (cond
       ((or (eq (char-before) ? ) ; At prompt start?
            (eq (char-before) ?~) ; Restart at $HOME
            (eq (char-before) ?:) ; Looks like URL
            (looking-back "[[:alnum:]]+:/" 3)) ; URL
        (insert "/"))
       ((> selectrum--actual-num-candidates-displayed 0)
        (selectrum-insert-current-candidate))
       (t (insert "/")))
    (insert "/")))

(defun pjones:selectrum-toggle-marginalia ()
  "Toggle `marginalia-mode' based on `selectrum-display-style'."
  (if (eq selectrum-display-style 'horizontal)
      (marginalia-mode -1)
    (marginalia-mode +1)))
(advice-add
 'selectrum-cycle-display-style
 :after #'pjones:selectrum-toggle-marginalia)

(let ((map selectrum-minibuffer-map))
  (define-key map (kbd "/") #'pjones:selectrum-slash)
  (define-key map (kbd "<right>") #'selectrum-next-candidate)
  (define-key map (kbd "<left>") #'selectrum-previous-candidate)
  (define-key map (kbd "<tab>") #'selectrum-next-candidate)
  (define-key map (kbd "<backtab>") #'selectrum-previous-candidate)
  (define-key map (kbd "C-<tab>") #'embark-collect-completions)
  (define-key map (kbd "C-`") #'embark-become)
  (define-key map (kbd "C-<return>") #'embark-act)
  (define-key map (kbd "M-a") #'marginalia-cycle))

;;; selectrum-conf.el ends here
