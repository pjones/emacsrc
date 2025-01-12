;;; consult-recoll-conf.el -- Settings for `consult-recoll' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'consult-recoll)
(require 'embark)

(defvar pjones:consult-recoll-strip-prefix
  (concat "file://" (expand-file-name "~/"))
  "File name prefix to remove from completion candidates.")

(defun pjones:consult-recoll-format-candidate (title url mime-type)
  "Format completion candidates for `consult-recoll'.
TITLE, URL, and MIME-TYPE are given by `consult-recoll'."
  (let* ((u (replace-regexp-in-string
             (rx (literal pjones:consult-recoll-strip-prefix)) "" url)))
    (format "%s (%s, %s)"
            (propertize title 'face 'consult-recoll-title-face)
            (propertize u 'face 'consult-recoll-url-face)
            (propertize mime-type 'face 'consult-recoll-mime-face))))

(custom-set-variables
 '(consult-recoll-prompt "Recoll: ")
 '(consult-recoll-group-by-mime nil)
 '(consult-recoll-format-candidate #'pjones:consult-recoll-format-candidate))

(consult-recoll-embark-setup)

;;; consult-recoll-conf.el ends here
