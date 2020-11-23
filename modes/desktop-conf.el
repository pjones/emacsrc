;;; desktop-conf.el --- Settings for `desktop-save-mode'

;;; Commentary:
;;
;; Settings for `desktop-save-mode'.
;;
;; Also see lisp/server.el

;;; Code:
(require 'desktop)

(custom-set-variables
 '(desktop-save t)
 '(desktop-load-locked-desktop t)
 `(desktop-dirname ,user-emacs-directory))

;;; desktop-conf.el ends here
