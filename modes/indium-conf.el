;;; indium-conf.el --- Configuration for indium.el
(eval-when-compile
  (require 'indium))

(defun pjones:indium-eval-last-and-close ()
  "Eval the last expression and close indium-scratch."
  (interactive)
  (indium-eval-last-node nil)
  (delete-window))

(defun pjones:indium-repl-mode-hook ()
  "Configure indium-repl-mode."
  (let ((map indium-repl-mode-map))
    (define-key map (kbd "C-c C-l") 'indium-repl-clear-output)
    (define-key map (kbd "C-c C-p") 'indium-repl-inspect)
    (define-key map (kbd "C-c C-c") 'indium-scratch)))

(defun pjones:indium-interaction-mode-hook ()
  "Configure indium-interaction-mode."
  (let ((map indium-interaction-mode-map))
    (define-key map (kbd "C-M-x")   'indium-eval-last-node)
    (define-key map (kbd "C-x C-s") 'indium-eval-last-node)
    (define-key map (kbd "C-c C-c") 'pjones:indium-eval-last-and-close)
    (define-key map (kbd "C-c C-p") 'indium-inspect-last-node)))

;; Install Hooks:
(add-hook 'indium-repl-mode-hook 'pjones:indium-repl-mode-hook)
(add-hook 'indium-interaction-mode-hook 'pjones:indium-interaction-mode-hook)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
