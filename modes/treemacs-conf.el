;;; treemacs-conf.el -- Settings for treemacs
;;
;;; Commentary:
;;
;;; Code:
(require 'treemacs)
(require 'treemacs-evil)
(require 'treemacs-projectile)

(custom-set-variables
 '(treemacs-project-follow-cleanup t)
 '(treemacs-is-never-other-window t)
 '(treemacs-follow-after-init t))

(defun pjones:treemacs-mode-hook ()
  "Hook for `treemacs-mode'."
  (treemacs-git-mode 'simple))

(add-to-list 'treemacs-mode-hook #'pjones:treemacs-mode-hook)
(add-to-list 'treemacs-mode-hook #'treemacs-filewatch-mode)
(add-to-list 'treemacs-mode-hook #'treemacs-follow-mode)
(add-to-list 'treemacs-mode-hook #'treemacs-fringe-indicator-mode)

;;; treemacs-conf.el ends here
