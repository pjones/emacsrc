;;; nix-mode-conf.el --- Settings for nix-mode
;;
;;; Commentary:
;;
;;; Code:

(require 'nix-mode)
(require 'reformatter)

(declare-function pjones:prog-mode-hook "../lisp/code.el")

(custom-set-variables
 '(nix-indent-function #'smie-indent-line)
 '(nix-mode-use-smie t))

(reformatter-define nix-format
  :program "nixpkgs-fmt"
  :group 'nix-mode)

(defun pjones:nix-mode-hook ()
  "Configure `nix-mode'."
  (pjones:prog-mode-hook)
  (nix-format-on-save-mode))

(define-key nix-mode-map (kbd "<return>") #'newline-and-indent)
(add-hook 'nix-mode-hook #'pjones:nix-mode-hook)

;;; nix-mode-conf.el ends here
