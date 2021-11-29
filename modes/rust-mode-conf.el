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

(add-hook 'rust-mode-hook #'rust-format-on-save-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;; rust-mode-conf.el ends here
