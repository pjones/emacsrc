;;; nix-mode-conf.el --- Settings for nix-mode
;;
;;; Commentary:
;;
;;; Code:

(require 'nix-mode)
(require 'reformatter)

(custom-set-variables
 '(nix-indent-function #'smie-indent-line))

(reformatter-define nix-format
  :program "nixpkgs-fmt"
  :group 'nix-mode)

(defun pjones:nix-mode-hook ()
  "Configure `nix-mode'."
  (pjones:prog-mode-hook)
  (electric-indent-local-mode)
  (nix-format-on-save-mode))

(define-key nix-mode-map (kbd "<return>") #'newline-and-indent)
(add-hook 'nix-mode-hook #'pjones:nix-mode-hook)

;;; nix-mode-conf.el ends here
