;;; sgml-mode-conf.el -- Settings for sgml-mode and its children (html-mode).
;;; Commentary:
;;; Code:
(eval-when-compile
  (require 'company))

(defun pjones:sgml-mode-hook ()
  "Configure `sgml-mode' (and things like `html-mode')."
  ;; Completion:
  (make-local-variable 'company-backends)
  (add-to-list 'company-backends 'company-nxml))

(declare-function pjones:add-programming-hook "code.el")
(pjones:add-programming-hook 'sgml-mode-hook)
(add-hook 'sgml-mode-hook 'pjones:sgml-mode-hook)

(provide 'sgml-mode-conf)
;;; sgml-mode-conf.el ends here
