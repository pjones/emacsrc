;;; eglot-conf.el -- Settings for eglot
;;
;;; Commentary:
;;
;;; Code:
(require 'eglot)

(custom-set-variables
 '(eglot-autoshutdown t))

(defun pjones:eglot--snippet-expansion-fn ()
  "Keep eglot from using yasnippet."
  nil)
(advice-add
 'eglot--snippet-expansion-fn
 :override #'pjones:eglot--snippet-expansion-fn)

;;; eglot-conf.el ends here
