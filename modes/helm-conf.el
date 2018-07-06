;;; helm-conf.el -- Settings for Helm.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (require 'helm))

;; Dependencies:
(require 'cl-lib)

;; Functions:
(defun pjones:helm-displayed-buffers-last (buffers _source)
  "Place visible buffers last in BUFFERS.

This function is meant to be used as a filtered-candidate-transformer."
  (let (visible)
    (append
     (cl-loop for i in buffers
              for buffer = (get-buffer i)
              for shown  = (get-buffer-window buffer t)
              if shown
              do (setq visible (cons i visible))
              else collect i)
     visible)))

;; Settings:
(custom-set-variables
 '(helm-ff-fuzzy-matching t)
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line nil)
 '(helm-follow-mode-persistent t)
 '(helm-source-names-using-follow '("Buffers"))
 '(helm-show-action-window-other-window 'right)
 `(helm-candidate-separator ,(make-string 78 ?─)))

;;; helm-conf.el ends here
