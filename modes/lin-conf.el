;;; lin-conf.el -- Settings for `lin' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'lin)

(defun pjones:lin-in-vertico ()
  "Enable lin in the minibuffer."
  (face-remap-add-relative 'vertico-current lin-face))

(custom-set-variables
 '(lin-face 'lin-mac))

(add-hook 'minibuffer-mode-hook #'pjones:lin-in-vertico)

;;; lin-conf.el ends here
