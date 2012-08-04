;;; dired-conf.el -- Settings for dired-mode
(eval-when-compile
  (require 'dired)
  (require 'dired-aux))

(setq dired-listing-switches "-lRA --ignore='.git' --group-directories-first"
      dired-auto-revert-buffer t
      dired-isearch-filenames t)
