;;; orgshow-conf.el -- Configuration for orgshow-mode.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'orgshow))

(defun pjones:orgshow-mode-hook ()
  (define-key orgshow-mode-map (kbd "<prior>")  'orgshow-prev)
  (define-key orgshow-mode-map (kbd "<next>") 'orgshow-next))

(add-hook 'orgshow-mode-hook 'pjones:orgshow-mode-hook)
