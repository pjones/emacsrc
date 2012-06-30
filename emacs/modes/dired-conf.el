;;; dired-conf.el -- Settings for dired-mode
(eval-when-compile
  (require 'dired)
  (require 'dired-aux))
(setq dired-listing-switches "-l"
      dired-auto-revert-buffer t
      dired-isearch-filenames t)
