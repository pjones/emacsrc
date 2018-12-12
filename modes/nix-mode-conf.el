;;; nix-mode-conf.el --- Settings for nix-mode
;;
;;; Commentary:
;;
;;; Code:
(require 'nix-mode)

(custom-set-variables
 '(nix-indent-function #'nix-indent-line))

(defun pjones:nix-mode-hook ()
  "Configure `nix-mode'."
  (make-local-variable 'electric-pair-open-newline-between-pairs)
  (setq electric-pair-open-newline-between-pairs nil))

(add-hook 'nix-mode-hook #'pjones:nix-mode-hook)

;;; nix-mode-conf.el ends here
