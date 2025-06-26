;;; org-grader-conf.el -- Settings for `org-grader' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-grader)

(declare-function pjones:org-hide-others "./org-conf")
(defvar org-grader-template-point)

(defun pjones:org-grader-mode-hook ()
  "Hook for `org-grader-mode'."
  (keymap-local-set "C-c i" #'org-grader-template-insert))

(defun pjones:org-grader-after-insert-hook ()
  "Hook for `org-grader-after-insert-hook'."
  (org-narrow-to-subtree)
  (when org-grader-template-point
    (goto-char org-grader-template-point))
  (pjones:org-hide-others))

(add-hook 'org-grader-mode-hook #'pjones:org-grader-mode-hook)
(add-hook 'org-grader-after-insert-hook #'pjones:org-grader-after-insert-hook)

;;; org-grader-conf.el ends here
