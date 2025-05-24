;;; org-grader-conf.el -- Settings for `org-grader' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-grader)

(defun pjones:org-grader-mode-hook ()
  "Hook for `org-grader-mode'."
  (keymap-local-set "C-c i" #'org-grader-template-insert))

(add-hook 'org-grader-mode-hook #'pjones:org-grader-mode-hook)

;;; org-grader-conf.el ends here
