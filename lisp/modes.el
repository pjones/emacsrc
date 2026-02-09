;;; modes.el -- Load files from ~/.emacs.d/pjones/modes on demand. -*- lexical-binding: t -*-
;;
;;; Commentary:
;;
;;; Code:

(declare-function envrc-global-mode "envrc")
(declare-function global-diff-hl-mode "diff-hl")
(declare-function global-hl-todo-mode "hl-todo")
(declare-function global-jinx-mode "jinx")
(declare-function minions-mode "minions")
(declare-function pdf-tools-install "pdf-tools")
(declare-function puni-global-mode "puni")
(declare-function vertico-mode "vertico")
(declare-function which-key-mode "which-key")
(declare-function winum-mode "winum")
(declare-function yas-global-mode "yasnippet")

(defvar pjones:modes-dir
  (concat (file-name-directory (directory-file-name (file-name-directory load-file-name))) "modes/")
  "The directory where I keep mode-specific configuration files.")

(defvar pjones:mode-aliases
  '((cc-mode c-ts-mode c++-ts-mode)
    (jupyter jupyter-repl)
    (lua-mode lua-ts-mode)
    (nix-mode nix-ts-mode)
    (ruby-mode ruby-ts-mode)
    (rust-mode rust-ts-mode)
    (yaml-mode yaml-ts-mode))
  "A list of aliases for modes.

Some modes should trigger the loading of one of my configuration files
even when the names don't match exactly.  For example, loading
`nix-ts-mode' should trigger a load of my `nix-mode-conf.el' file.")

;; Automatically load my per-mode configuration files
(dolist (file (directory-files pjones:modes-dir t))
  (let ((basename (file-name-nondirectory file)))
    (when (string-match "\\(-conf\\.elc\\)$" basename)
      (eval-after-load (intern (replace-match "" t t basename))
        `(load ,file)))))

(dolist (alias pjones:mode-aliases)
  (let* ((mode (symbol-name (car alias)))
         (file (concat pjones:modes-dir mode "-conf.elc")))
    (dolist (other (cdr alias))
      (eval-after-load other
        `(load ,file)))))

(defun pjones:basic-mode-hook ()
  "Minor modes to enable for most major modes."
  (font-lock-mode)
  (auto-fill-mode)
  (line-number-mode)
  (column-number-mode)
  (size-indication-mode))

(defun pjones:boot-global-modes ()
  "Start or prepare global modes."
  (editorconfig-mode)                ; EditorConfig Plugin
  (electric-pair-mode)               ; Insert matching brackets.
  (envrc-global-mode)                ; Respect .envrc files.
  (global-auto-revert-mode)          ; External changes cause a revert
  (global-diff-hl-mode)              ; Show what changes in a buffer
  (global-hl-todo-mode)              ; Highlight TODO tags in text.
  (global-jinx-mode)                 ; Enchanted Spell Checker
  (global-prettify-symbols-mode)     ; Replace buffer symbols.
  (minions-mode)                     ; Minor-mode menu.
  (repeat-mode)                      ; Easy repeating of some actions.
  (vertico-mode)                     ; minibuffer completions.
  (which-key-mode)                   ; Remind me what keys do.
  (winner-mode)                      ; Track win conifg changes
  (winum-mode)                       ; Number windows

  ;; PDF files should use pdf-tools!
  (use-package pdf-tools
    :magic ("%PDF" . pdf-view-mode)
    :config (pdf-tools-install :no-query))

  ;; Seems to only work this way:
  (use-package treesit-auto
    :config
    (global-treesit-auto-mode))

  ;; Libraries used throughout my Emacs session:
  (require 'saveplace)                    ; Saves your location in files
  (require 'vlf-setup)                    ; Deal with large files

  ;; Load my Meow config and start it:
  (require 'meow)

  (add-hook 'text-mode-hook #'abbrev-mode))

;; Hook in:
(unless noninteractive
  (add-hook 'prog-mode-hook #'pjones:basic-mode-hook)
  (add-hook 'text-mode-hook #'pjones:basic-mode-hook)
  (add-hook 'after-init-hook #'pjones:boot-global-modes))

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:

;;; modes.el ends here
