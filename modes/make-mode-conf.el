;;; make-mode-conf.el -- Settings for makefile-mode in make-mode.el.
(eval-when-compile (require 'make-mode))

(defun pjones:makefile-mode-hook ()
  "Fix a few things in make files that are different that the
settings in my prog-mode configuration."
  (local-set-key (kbd "RET") 'newline-and-indent)
  (setq indent-tabs-mode t))

(add-hook 'makefile-mode-hook 'pjones:makefile-mode-hook)
