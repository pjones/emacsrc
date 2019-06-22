;;; magit-conf.el -- Customizations for magit.
;;
;;; Commentary:
;;
;;; Code:
(require 'magit)
(require 'forge)

(custom-set-variables
 '(magit-popup-use-prefix-argument 'default)
 '(magit-status-margin '(t age magit-log-margin-width nil 18))
 '(magit-status-show-hashes-in-headers t)
 '(magit-section-initial-visibility-alist
   '(([unpushed status] . show)))

 ;; Evil
 '(evil-magit-want-horizontal-movement t)
 '(evil-magit-use-z-for-folds t)
 '(evil-magit-state 'normal))

;; Load evil-magin:
(require 'evil)
(require 'evil-magit)
(add-hook 'git-commit-setup-hook #'evil-insert-state)

;;; magit-conf.el ends here
