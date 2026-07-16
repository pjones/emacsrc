;;; js-conf.el -- Configuration options for js-mode (JavaScript). -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'reformatter)
(require 'js)

(reformatter-define js-format
  :program "prettier"
  :args '("--parser" "babel")
  :group 'js-mode)

(defun pjones:js-mode-hook ()
  "Set up `js-mode'."
  (js-format-on-save-mode 1))

;; JavaScript mode settings
(custom-set-variables
 '(js-indent-level 2)
 '(js-flat-functions t))

(add-to-list 'js-mode-hook #'pjones:js-mode-hook)
(add-to-list 'js-ts-mode-hook #'pjones:js-mode-hook)

;;; js-conf.el ends here
