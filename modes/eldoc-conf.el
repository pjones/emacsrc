;;; eldoc-conf.el -- Settings for `eldoc' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'eldoc)

(custom-set-variables
  '(eldoc-echo-area-display-truncation-message nil)
  '(eldoc-echo-area-use-multiline-p t)
  '(eldoc-echo-area-prefer-doc-buffer 'maybe)
  '(eldoc-help-at-pt t)
  '(eldoc-documentation-strategy #'eldoc-documentation-enthusiast))

;;; eldoc-conf.el ends here
