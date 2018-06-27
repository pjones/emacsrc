;;; helm-conf.el -- Settings for Helm.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'helm))

;; Settings:
(custom-set-variables
 '(helm-ff-fuzzy-matching nil)
 '(helm-echo-input-in-header-line nil)
 '(helm-follow-mode-persistent t)
 '(helm-source-names-using-follow '("Buffers"))
 '(helm-show-action-window-other-window 'right)
 `(helm-candidate-separator ,(make-string 78 ?─)))

;;; helm-conf.el ends here
