;;; vterm-conf.el -- Settings for `vterm' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'vterm)

(declare-function puni-mode "puni")

(custom-set-variables
  '(vterm-buffer-name-string "vterm %s"))

(defun pjones:vterm-mode-hook ()
  "Mode hook for `vterm-mode'."
  (puni-mode -1)) ; Disable puni mode.

(add-hook 'vterm-mode-hook #'pjones:vterm-mode-hook)

;;; vterm-conf.el ends here
