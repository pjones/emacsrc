;;; font-lock-conf.el -- Settings for `font-lock' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'font-lock)

;; Create some faces
(defface pjones:fixme-face
  '((t (:inherit 'font-lock-warning-face)))
  "Face to style FIXME and TODO with."
  :group 'faces)

(defun pjones:font-lock-add-fixme ()
  "Add todo markers as keywords."
  (font-lock-add-keywords nil '(("\\<\\(FIXME:\\|TODO:\\|NOTE:\\)"
                                 1 'pjones:fixme-face t))))

(add-hook 'font-lock-mode-hook #'pjones:font-lock-add-fixme)

;;; font-lock-conf.el ends here
