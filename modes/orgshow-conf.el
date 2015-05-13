;;; orgshow-conf.el -- Configuration for orgshow-mode.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'orgshow))

(defun pjones:orgshow-mode-hook ()
  (define-key orgshow-mode-map (kbd "<left>")  'orgshow-prev)
  (define-key orgshow-mode-map (kbd "<right>") 'orgshow-next))

(add-hook 'orgshow-mode-hook 'pjones:orgshow-mode-hook)
