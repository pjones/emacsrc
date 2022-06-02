;;; elec-pair-conf.el -- Settings for `elec-pair' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'elec-pair)

(defun pjones:electric-pair-open-newline-between-pairs-psif (&rest _)
  "Advice to fix bad indentation caused by in `electric-pair-mode'."
  (save-excursion
    (move-beginning-of-line 1)
    (when (looking-at-p "[ \t]*$")
      (forward-line)
      (indent-according-to-mode))))

(advice-add
 #'electric-pair-open-newline-between-pairs-psif
 :after #'pjones:electric-pair-open-newline-between-pairs-psif)

;;; elec-pair-conf.el ends here
