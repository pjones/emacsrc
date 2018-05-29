;;; helm-conf.el -- Settings for Helm.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'helm))

(custom-set-variables
 '(helm-echo-input-in-header-line t)
 '(helm-follow-mode-persistent t)
 `(helm-candidate-separator ,(make-string 78 ?─)))

;;; Hooks:
(add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

;;; helm-conf.el ends here
