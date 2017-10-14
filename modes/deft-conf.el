;;; deft-conf.el -- Settings for deft-mode.
(eval-when-compile
  (require 'deft))


(custom-set-variables
  `(deft-directory ,(expand-file-name "~/notes/"))
  '(deft-recursive t)
  '(deft-extensions (quote ("md" "txt" "org")))
  '(deft-auto-save-interval 0))
