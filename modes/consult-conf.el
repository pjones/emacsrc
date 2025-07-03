;;; consult-conf.el -- Settings for `consult' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'consult)
(require 'project)

;; Make the linting tool happy:
(defvar consult-imenu-config)

(defun pjones:consult-project-root-function ()
  "Return the current project's root directory."
  (when-let ((project (project-current)))
    (project-root project)))

(custom-set-variables
 '(consult-project-root-function #'pjones:consult-project-root-function))

(with-eval-after-load 'consult-imenu
  (setopt consult-imenu-config
          (append consult-imenu-config
                  '((dired-mode :toplevel "Files"
                                :types ((?f "Files" font-lock-variable-name-face)
                                        (?d "Directories" font-lock-type-face)))))))

;;; consult-conf.el ends here
