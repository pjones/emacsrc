;;; consult-conf.el -- Settings for `consult' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'consult)
(require 'project)

(defun pjones:consult-project-root-function ()
  "Return the current project's root directory."
  (when-let ((project (project-current)))
    (project-root project)))

(custom-set-variables
 '(consult-project-root-function #'pjones:consult-project-root-function))

;;; consult-conf.el ends here
