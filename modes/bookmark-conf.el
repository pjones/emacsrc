;;; bookmark-conf.el -- Settings for `bookmark'
;;
;;; Commentary:
;;
;;; Code:
(require 'bookmark)

(custom-set-variables
 '(bookmark-default-file (concat user-emacs-directory "bookmarks"))
 '(bookmark-watch-bookmark-file 'silent) ; Auto reload bookmarks
 '(bookmark-save-flag 0)                 ; Save bookmarks as they change
 '(bookmark-version-control 'never))     ; Don't make backup copies

;;; bookmark-conf.el ends here
