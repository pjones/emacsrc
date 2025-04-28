;;; rust-mode-conf.el -- Settings for `rust-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'reformatter)
(require 'rust-mode)

(custom-set-variables
 '(rust-format-on-save nil))

(reformatter-define rust-format
  :program "rustfmt"
  :group 'rust-mode)

(defun pjones:rust-mode-hook ()
  "Set up `rust-mode' buffers."
  (eglot-ensure)
  (rust-format-on-save-mode))

(add-hook 'rust-mode-hook #'pjones:rust-mode-hook)
(add-hook 'rust-ts-mode-hook #'pjones:rust-mode-hook)

;;; rust-mode-conf.el ends here
