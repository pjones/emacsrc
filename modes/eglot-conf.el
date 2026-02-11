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

;; clangd needs to know where the commands file is so that it picks
;; the correct language version.  In this case I always use the
;; "build" directory for this.
(add-to-list
 'eglot-server-programs
 '((c-mode c-ts-mode c++-mode c++-ts-mode ) .
     ("clangd" "--compile-commands-dir=build")))

;;; eglot-conf.el ends here
