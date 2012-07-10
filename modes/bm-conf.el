;;; bm-conf.el -- Visual bookmarks configuration.
(eval-when-compile
  (load "../lisp/packages.el")
  (require 'bm))

(setq bm-highlight-style 'bm-highlight-only-fringe
      bm-restore-repository-on-load nil)