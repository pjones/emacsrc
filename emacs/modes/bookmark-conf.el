;;; bookmark-conf.el -- Settings for Emacs bookmarks.
(eval-when-compile (require 'bookmark))
(setq bookmark-default-file (concat user-emacs-directory "bookmarks")
      bookmark-save-flag 0              ; Save bookmarks as they change
      bookmark-version-control 'never)  ; Don't make backup copies
