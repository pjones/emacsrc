;;; eglot-conf.el -- Settings for eglot -*- lexical-binding: t -*-
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

;; Don't reformat buffers while I'm typing:
;; https://chaos.social/@root42/113548269273998426
(add-to-list
 'eglot-ignored-server-capabilities
 :documentOnTypeFormattingProvider)

;;; eglot-conf.el ends here
