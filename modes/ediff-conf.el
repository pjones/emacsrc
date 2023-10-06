;;; ediff-conf.el -- Settings for `ediff' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'ediff)

(custom-set-variables
  '(ediff-window-setup-function 'ediff-setup-windows-plain)
  '(ediff-split-window-function 'split-window-horizontally)
  '(ediff-make-buffers-readonly-at-startup t))

;;; ediff-conf.el ends here
