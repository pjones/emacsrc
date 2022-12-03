;;; mini-frame-conf.el -- Settings for `mini-frame' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'mini-frame)
(require 'posframe)

;; Adapted from: https://github.com/muffinmad/emacs-mini-frame/issues/50
;;
;; no-accept-focus: https://github.com/minad/vertico/issues/115#issuecomment-973753577
(defun pjones:mini-frame-position ()
  "Return the desired position of the min-frame."
  (let* ((info (posframe-poshandler-argbuilder))
         (posn (posframe-poshandler-point-bottom-left-corner info)))
    `((left . ,(car posn))
      (top . ,(cdr posn))
      (no-accept-focus . t))))

(custom-set-variables
  '(mini-frame-show-parameters #'pjones:mini-frame-position)
  '(mini-frame-completions-show-parameters #'pjones:mini-frame-position))

;; Don't use a mini-frame for these functions:
(add-to-list 'mini-frame-ignore-commands 'consult-ripgrep)
(add-to-list 'mini-frame-ignore-commands 'org-completing-read)
(add-to-list 'mini-frame-ignore-commands 'org-insert-link)

;;; mini-frame-conf.el ends here
