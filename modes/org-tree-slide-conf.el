;;; org-tree-slide-conf.el -- Settings for `org-tree-slide' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'org-tree-slide)

(custom-set-variables
 '(org-tree-slide-activate-message nil)
 '(org-tree-slide-deactivate-message nil))

(let ((map org-tree-slide-mode-map))
  (define-key map (kbd "M-n") #'org-tree-slide-move-next-tree)
  (define-key map (kbd "M-p") #'org-tree-slide-move-previous-tree))

(defun pjones:org-tree-slide-after-narrow-hook ()
  "Hook called at slideshow start."
  (org-display-inline-images nil t))

(add-hook 'org-tree-slide-after-narrow-hook
          #'pjones:org-tree-slide-after-narrow-hook)

;;; org-tree-slide-conf.el ends here
