;;; org-grader-conf.el -- Settings for `org-grader' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-grader)

(declare-function pjones:org-hide-others "./org-conf")
(defvar org-grader-mode-map)
(defvar org-grader-template-point)

(defun pjones:org-grader-mode-hook ()
  "Hook for `org-grader-mode'."
  (keymap-set org-grader-mode-map "C-c i" #'org-grader-template-insert))

(add-hook 'org-grader-mode-hook #'pjones:org-grader-mode-hook)

;;; org-grader-conf.el ends here
