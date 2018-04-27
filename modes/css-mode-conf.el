;;; css-mode-conf.el -- Settings for css-mode.
(eval-when-compile
  (require 'css-mode)
  (require 'company))

(declare-function pjones:add-programming-hook "code.el")

(defun pjones:css-mode-hook ()
  "Settings and overrides for css-mode."
  ;; Completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-css)

  ;; Other modes:
  (rainbow-mode)

  (setq css-indent-offset 2)
  (add-hook 'after-save-hook 'pjones:after-save-reload-browser nil t))

(pjones:add-programming-hook 'css-mode-hook)
(add-hook 'css-mode-hook 'pjones:css-mode-hook)
