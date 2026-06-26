;;; eglot-conf.el -- Settings for eglot -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'eglot)

(defun pjones:eglot-managed-mode-hook nil
  "Hook function for `eglot-managed-mode-hook'."
  ;; I don't want this (too noisy):
  (eglot-inlay-hints-mode -1))

(custom-set-variables
 '(eglot-autoshutdown t)
 '(eglot-code-action-indications '(margin)))

 ;; Better function argument descriptions:
(require 'eglot-signature-eldoc-talkative)
(advice-add #'eglot-signature-eldoc-function
  :override #'eglot-signature-eldoc-talkative)

;; I don't want magic snippet expansion.
(advice-add
 'eglot--snippet-expansion-fn
 :override #'ignore)

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

(add-hook 'eglot-managed-mode-hook #'pjones:eglot-managed-mode-hook)

;;; eglot-conf.el ends here
