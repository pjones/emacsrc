;;; magit-conf.el -- Customizations for magit.
;;
;;; Commentary:
;;
;;; Code:
(eval-when-compile
  (defvar magit-last-seen-setup-instructions)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (require 'magit))

(custom-set-variables
 '(magit-popup-use-prefix-argument 'default)
 '(magit-status-margin '(t age magit-log-margin-width nil 18))
 '(magit-status-show-hashes-in-headers t)

 ;; Evil
 '(evil-magit-want-horizontal-movement t)
 '(evil-magit-use-z-for-folds t)
 '(evil-magit-state 'normal))

;; Load evil-magin:
(require 'evil-magit)

;;; magit-conf.el ends here
