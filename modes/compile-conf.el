;;; compile-conf.el -- Settings for `compile' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'compile)

(declare-function pjones:display-buffer-in-non-popup-frame "../lisp/functions")

(custom-set-variables
  '(compilation-auto-jump-to-first-error t)
  '(compilation-always-kill t))

(defun pjones:compile-goto-error (orig &rest args)
  "Call ORIG with ARGS, keeping it from using a popup frame."
  (let ((buf (current-buffer))
        (win (selected-window))
        (ret (apply orig args))
        (new nil))
    (setq new (current-buffer))
    (set-window-buffer win buf)
    (pjones:display-buffer-in-non-popup-frame new)
    ret))

(advice-add 'compile-goto-error :around #'pjones:compile-goto-error)

;;; compile-conf.el ends here
