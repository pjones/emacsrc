;;; helm-conf.el -- Settings for Helm.

(custom-set-variables
 '(helm-mode-fuzzy-match t)
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-display-function #'helm-default-display-buffer)
 '(helm-display-buffer-width 100)
 '(helm-display-buffer-height 20)
 `(helm-candidate-separator ,(make-string 70 ?─)))
