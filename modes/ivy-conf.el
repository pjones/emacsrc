;;; ivy-conf.el -- ido replacement tool.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'ivy))

(custom-set-variables
 '(counsel-find-file-at-point t)
 '(ivy-wrap t)
 '(ivy-tab-space nil)
 '(ivy-initial-inputs-alist nil)
 '(ivy-re-builders-alist
      '((counsel-describe-function . ivy--regex-plus)
        (counsel-describe-variable . ivy--regex-plus)
        (t                         . ivy--regex-fuzzy))))

(defun pjones:ivy-better-tab ()
  "Fully complete the selected completion candidate."
  (interactive)
  (ivy-insert-current)
  (ivy-partial))

(let ((map ivy-minibuffer-map))
  (define-key map (kbd "TAB")   'pjones:ivy-better-tab)
  (define-key map (kbd "C-j")   'ivy-immediate-done)
  (define-key map (kbd "C-m")   'ivy-done)
  (define-key map (kbd "C-w")   'ivy-backward-kill-word)
  (define-key map (kbd "C-c h") 'hydra-ivy/body))

;; (provide 'ivy-config)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
