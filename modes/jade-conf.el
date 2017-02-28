;;; jade-conf.el --- Configuration for jade.el
(eval-when-compile
  (require 'jade))

(defun pjones:jade-eval-last-and-close ()
  "Eval the last expression and close jade-scratch."
  (interactive)
  (jade-eval-last-node nil)
  (delete-window))

(defun pjones:jade-repl-mode-hook ()
  "Configure jade-repl-mode."
  (let ((map jade-repl-mode-map))
    (define-key map (kbd "C-c C-l") 'jade-repl-clear-output)
    (define-key map (kbd "C-c C-p") 'jade-repl-inspect)
    (define-key map (kbd "C-c C-c") 'jade-scratch)))

(defun pjones:jade-interaction-mode-hook ()
  "Configure jade-interaction-mode."
  (let ((map jade-interaction-mode-map))
    (define-key map (kbd "C-M-x")   'jade-eval-last-node)
    (define-key map (kbd "C-x C-s") 'jade-eval-last-node)
    (define-key map (kbd "C-c C-c") 'pjones:jade-eval-last-and-close)
    (define-key map (kbd "C-c C-p") 'jade-inspect-last-node)))

;; Install Hooks:
(add-hook 'jade-repl-mode-hook 'pjones:jade-repl-mode-hook)
(add-hook 'jade-interaction-mode-hook 'pjones:jade-interaction-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
