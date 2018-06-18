;;; ediff-conf.el -- Settings for Ediff.
(eval-when-compile
  (require 'ediff)
  (require 'ediff-wind))

(custom-set-variables
  '(ediff-window-setup-function 'ediff-setup-windows-plain)
  '(ediff-split-window-function 'split-window-horizontally)
  '(ediff-make-buffers-readonly-at-startup t))

;;; ediff-conf.el ends here
