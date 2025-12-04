;;; nix-mode-conf.el -- Settings for `nix-mode' -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'nix-mode)
(require 'reformatter)

;; Hide the following functions from `nix-flake' from M-x because I
;; don't want to use them that way.
(dolist (symbol '(nix-flake
                  nix-flake-build-attribute
                  nix-flake-build-default
                  nix-flake-check
                  nix-flake-dispatch
                  nix-flake-dispatch
                  nix-flake-init
                  nix-flake-init-dispatch
                  nix-flake-init-select-template
                  nix-flake-lock
                  nix-flake-run-attribute
                  nix-flake-run-default
                  nix-flake-update))
  (put symbol 'completion-predicate #'ignore))

(custom-set-variables
 '(nix-indent-function #'smie-indent-line)
 '(nix-mode-use-smie t))

(reformatter-define nix-format
  :program "nixfmt"
  :group 'nix-mode)

(defun pjones:nix-mode-hook ()
  "Configure `nix-mode'."
  (eglot-ensure)
  (when (fboundp 'pjones:prog-mode-hook)
    (pjones:prog-mode-hook))
  (nix-format-on-save-mode))

(add-hook 'nix-mode-hook #'pjones:nix-mode-hook)
(add-hook 'nix-ts-mode-hook #'pjones:nix-mode-hook)

;;; nix-mode-conf.el ends here
