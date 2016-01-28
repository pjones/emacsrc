;;; ivy-conf.el -- Incremental Vertical completion.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'ivy))

(setq ivy-wrap t
      ivy-re-builders-alist '((t . ivy--regex-fuzzy))
      ivy-initial-inputs-alist nil)

(define-key ivy-minibuffer-map (kbd "C-w")   'ivy-backward-kill-word)
(define-key ivy-minibuffer-map (kbd "C-h")   'hydra-ivy/body)
(define-key ivy-minibuffer-map (kbd "C-c h") 'hydra-ivy/body)
