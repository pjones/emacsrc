;;; ediff-wind-conf.el -- Settings for Ediff Windows.
(eval-when-compile (require 'ediff-wind))
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)
