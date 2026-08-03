;;; compile-conf.el -- Settings for `compile' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'ansi-color)
(require 'compile)
(require 'project)

(custom-set-variables
 '(compilation-buffer-name-function #'pjones:compile-make-buffer-name)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-always-kill t))

(defun pjones:compile-make-buffer-name (name-of-mode)
  "Rename a `compile-mode' buffer with a project name.
Prefix the project name with NAME-OF-MODE."
  (let ((project (project-current nil)))
    (concat "*" (downcase name-of-mode)
            (if project (concat ":" (project-name project)) "")
            "*")))

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;; compile-conf.el ends here
