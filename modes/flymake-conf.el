;;; flymake-conf.el -- Settings for `flymake' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'flymake)

(custom-set-faces
 '(flymake-error ((t (:underline nil))))
 '(flymake-warning ((t (:underline nil)))))

(custom-set-variables
 '(flymake-show-diagnostics-at-end-of-line nil))

(defvar-keymap flymake-repeat-map
  :repeat t
  "n" #'flymake-goto-next-error
  "p" #'flymake-goto-prev-error)

;;; flymake-conf.el ends here
