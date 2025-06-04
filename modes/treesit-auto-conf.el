;;; treesit-auto-conf.el -- Settings for `treesit-auto' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'treesit-auto)

;; Don't use tree sitter for the following modes:
(dolist (mode '(bash))        ; Bad syntax highlighting.
  (delete mode treesit-auto-langs))

;;; treesit-auto-conf.el ends here
