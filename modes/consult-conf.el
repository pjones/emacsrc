;;; consult-conf.el -- Settings for `consult' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'consult)
(require 'project)

(custom-set-variables
  '(consult-project-root-function #'pjones:consult-project-root-function))

(defun pjones:consult-project-root-function ()
  "Return the current project's root directory."
  (when-let ((project (project-current)))
    (project-root project)))

;;; consult-conf.el ends here
