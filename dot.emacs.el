;;; dot.emacs.el -- Bootstrap Emacs.
;;
;;; Commentary:
;;
;; Emacs loads this file, which in turn loads all of my configuration.
;;
;;; Code:

;; Silence the linter/compiler:
(defvar gnus-directory)
(declare-function pjones:load-configuration-files "./lisp/loadpath")

;; Important load settings:
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      gnus-directory (expand-file-name "~/.cache/emacs/news"))

;; Load other configuration files
(load "@loadpathel@")
(pjones:load-configuration-files)

;;; dot.emacs.el ends here
