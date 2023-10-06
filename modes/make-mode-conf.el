;;; make-mode-conf.el -- Settings for `make-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'make-mode)

(defun pjones:makefile-mode-hook ()
  "Mode settings for `make-mode'."
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'pjones:makefile-mode-hook)

;;; make-mode-conf.el ends here
