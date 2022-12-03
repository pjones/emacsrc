;;; modes.el -- Load files from ~/.emacs.d/pjones/modes on demand.
;;
;;; Commentary:
;;
;;; Code:

(declare-function default-text-scale-mode "default-text-scale")
(declare-function envrc-global-mode "envrc")
(declare-function global-diff-hl-mode "diff-hl")
(declare-function mini-frame-mode "mini-frame")
(declare-function minions-mode "minions")
(declare-function pdf-tools-install "pdf-tools")
(declare-function projectile-mode "projectile")
(declare-function puni-global-mode "puni")
(declare-function vertico-mode "vertico")
(declare-function which-key-mode "which-key")
(declare-function winum-mode "winum")
(declare-function yas-global-mode "yasnippet")

(defvar pjones:modes-dir
  (concat (file-name-directory (directory-file-name (file-name-directory load-file-name))) "modes/")
  "The directory where I keep mode-specific configuration files.")

;; Automatically load my per-mode configuration files
(dolist (file (directory-files pjones:modes-dir t))
  (let ((basename (file-name-nondirectory file)))
    (when (string-match "\\(-conf\\.elc\\)$" basename)
      (eval-after-load (intern (replace-match "" t t basename))
        `(load ,file)))))

(defun pjones:basic-mode-hook ()
  "Minor modes to enable for most major modes."
  (font-lock-mode)
  (auto-fill-mode)
  (flyspell-mode)
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode))

(defun pjones:boot-global-modes ()
  "Start or prepare global modes."
  (default-text-scale-mode)          ; Frame text scaling.
  (electric-pair-mode)               ; Insert matching brackets.
  (envrc-global-mode)                ; Respect .envrc files.
  (ffap-bindings)                    ; Finding Files and URLs at Point
  (global-auto-revert-mode)          ; External changes cause a revert
  (global-diff-hl-mode)              ; Show what changes in a buffer
  (global-prettify-symbols-mode)     ; Replace buffer symbols.
  (mini-frame-mode)                  ; Mini-buffer in a frame
  (minions-mode)                     ; Minor-mode menu.
  (projectile-mode)                  ; Project tool
  (vertico-mode)                     ; minibuffer completions.
  (show-paren-mode)                  ; Highlight matching brackets.
  (which-key-mode)                   ; Remind me what keys do.
  (winner-mode)                      ; Track win conifg changes
  (winum-mode)                       ; Number windows
  (yas-global-mode)                  ; Snippets.

  ;; Force my overrides to apply:
  (require 'puni)
  (puni-global-mode) ; Working with delimiters.

  ;; Other modes that need to be activated:
  (pdf-tools-install)                     ; Internal PDF viewer

  ;; Libraries used throughout my Emacs session:
  (require 'saveplace)                    ; Saves your location in files
  (require 'vlf-setup)                    ; Deal with large files

  (add-hook 'text-mode-hook #'abbrev-mode))

;; Hook in:
(unless noninteractive
  (add-hook 'prog-mode-hook #'pjones:basic-mode-hook)
  (add-hook 'text-mode-hook #'pjones:basic-mode-hook)
  (add-hook 'after-init-hook #'pjones:boot-global-modes))

;;; modes.el ends here
