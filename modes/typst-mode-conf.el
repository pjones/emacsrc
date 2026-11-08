;;; typst-mode-conf.el -- Settings for `typst-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'eglot)
(require 'typst-ts-mode)

(defun pjones:typst-mode-hook ()
  "Set up `typst-ts-mode'."
  (auto-fill-mode -1)
  (visual-line-mode)
  (visual-wrap-prefix-mode)
  (eglot-ensure))

(custom-set-variables
 '(typst-ts-lsp-download-path "/dev/null")
 '(typst-ts-electric-return nil))

;; Ensure eglot can find the LSP server:
(add-to-list
 'eglot-server-programs
 '(((typst-ts-mode :language-id "typst")) . ("tinymist")))

(add-hook 'typst-ts-mode-hook #'pjones:typst-mode-hook)

;;; typst-mode-conf.el ends here
