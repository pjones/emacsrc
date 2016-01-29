;;; ivy-conf.el -- ido replacement tool.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'ivy))

(setq ivy-wrap t
      ivy-initial-inputs-alist nil
      ivy-re-builders-alist
      '((counsel-describe-function . ivy--regex-plus)
        (counsel-describe-variable . ivy--regex-plus)
        (t                         . ivy--regex-fuzzy)))

(let ((map ivy-minibuffer-map))
  (define-key map (kbd "TAB")   'ivy-partial)
  (define-key map (kbd "C-j")   'ivy-immediate-done)
  (define-key map (kbd "C-m")   'ivy-done)
  (define-key map (kbd "C-w")   'ivy-backward-kill-word)
  (define-key map (kbd "C-c h") 'hydra-ivy/body))

;; (provide 'ivy-config)
