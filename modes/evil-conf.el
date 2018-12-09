;;; evil-conf.el -- Settings for Evil.
;;
;;; Commentary:
;;
;;; Code:
(add-hook 'evil-mode-hook 'evil-commentary-mode)
(add-hook 'evil-mode-hook 'global-evil-fringe-mark-mode)
(add-hook 'evil-mode-hook 'evil-collection-init)

;;; evil-conf.el ends here
