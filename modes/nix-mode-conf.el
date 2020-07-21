;;; nix-mode-conf.el --- Settings for nix-mode
;;
;;; Commentary:
;;
;;; Code:
(require 'nix-mode)
(require 'reformatter)

(custom-set-variables
 '(nix-indent-function #'nix-indent-line))

(reformatter-define nix-format
  :program "nixpkgs-fmt")

(defun pjones:nix-mode-hook ()
  "Configure `nix-mode'."
  (setq-local indent-line-function #'indent-relative)
  (electric-indent-mode -1)
  (pjones:prog-mode-hook)
  (nix-format-on-save-mode))

(define-key nix-mode-map (kbd "<return>") #'newline-and-indent)
(add-hook 'nix-mode-hook #'pjones:nix-mode-hook)

;;; nix-mode-conf.el ends here
