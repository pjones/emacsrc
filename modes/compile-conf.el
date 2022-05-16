;;; compile-conf.el -- Settings for `compile' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ansi-color)
(require 'compile)

(declare-function pjones:display-buffer-in-non-popup-frame "../lisp/functions")
(declare-function pjones:frame-popup-p "../lisp/functions")

(custom-set-variables
  '(compilation-auto-jump-to-first-error nil)
  '(compilation-always-kill t))

(defun pjones:compile-goto-error (orig &rest args)
  "Call ORIG with ARGS, keeping it from using a popup frame."
  (let ((buf (current-buffer))
        (win (selected-window))
        ret new)
    (if (not (pjones:frame-popup-p (window-frame win)))
      (apply orig args)
      (setq ret (apply orig args)
            new (current-buffer))
      (set-window-buffer win buf)
      (pjones:display-buffer-in-non-popup-frame new)
      ret)))

(advice-add 'compile-goto-error :around #'pjones:compile-goto-error)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; compile-conf.el ends here
