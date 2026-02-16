;;; cc-mode-conf.el -- Settings for `cc-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'cc-mode)
(require 'eglot)
(require 'reformatter)

(declare-function indent-bars-mode "indent-bars")

(reformatter-define cc-format
  :program "clang-format"
  :group 'cc-mode)

(custom-set-variables
 '(c-basic-offset 2)
 '(c-default-style "bsd"))

(defun pjones:c-mode-hook ()
  "Set up C-like modes."
  (cc-format-on-save-mode)
  (indent-bars-mode)
  (eglot-ensure))

(dolist (hook '(c-mode-hook c-ts-mode-hook
                c++-mode-hook c++-ts-mode-hook
                objc-mode-hook))
  (add-hook hook #'pjones:c-mode-hook))

;;; cc-mode-conf.el ends here
