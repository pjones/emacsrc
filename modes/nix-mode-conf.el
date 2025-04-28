;;; nix-mode-conf.el -- Settings for `nix-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'nix-mode)
(require 'reformatter)

(custom-set-variables
 '(nix-indent-function #'smie-indent-line)
 '(nix-mode-use-smie t))

(reformatter-define nix-format
  :program "nixpkgs-fmt"
  :group 'nix-mode)

(defun pjones:nix-mode-hook ()
  "Configure `nix-mode'."
  (eglot-ensure)
  (when (fboundp 'pjones:prog-mode-hook)
    (pjones:prog-mode-hook))
  (nix-format-on-save-mode))

(add-hook 'nix-mode-hook #'pjones:nix-mode-hook)

;;; nix-mode-conf.el ends here
