;;; modes.el -- Load files from ~/.emacs.d/pjones/modes on demand.
;;
;;; Commentary:
;;
;;; Code:

(declare-function counsel-mode "counsel")
(declare-function default-text-scale-mode "default-text-scale")
(declare-function direnv-mode "direnv")
(declare-function global-diff-hl-mode "diff-hl")
(declare-function ivy-mode "ivy")
(declare-function minions-mode "minions")
(declare-function projectile-mode "projectile")
(declare-function shackle-mode "shackle")
(declare-function which-key-mode "which-key")
(declare-function winum-mode "winum")
(declare-function yas-global-mode "yasnippet")
(declare-function pdf-tools-install "pdf-tools")

(defvar pjones:modes-dir
  (concat (file-name-directory (directory-file-name (file-name-directory load-file-name))) "modes/")
  "The directory where I keep mode-specific configuration files.")

;; Automatically load my per-mode configuration files
(dolist (file (directory-files pjones:modes-dir t))
  (let ((basename (file-name-nondirectory file)))
    (when (string-match "\\(-conf\\.elc\\)$" basename)
      (eval-after-load (intern (replace-match "" t t basename))
        `(load ,file)))))

(defun pjones:boot-global-modes ()
  "Start or prepare global modes."
  (counsel-mode)                          ; More completion via Ivy
  (default-text-scale-mode)               ; Frame text scaling.
  (direnv-mode)                           ; Respect .envrc files.
  (global-auto-revert-mode)               ; External changes cause a revert
  (global-diff-hl-mode)                   ; Show what changes in a buffer
  (global-prettify-symbols-mode)          ; Replace buffer symbols.
  (ivy-mode)                              ; Minibuffer completion
  (minions-mode)                          ; Hide some minor modes.
  (projectile-mode)                       ; Project tool
  (shackle-mode)                          ; Control pop-up windows.
  (which-key-mode)                        ; Remind me what keys do.
  (winner-mode)                           ; Track win conifg changes
  (winum-mode)                            ; Number windows
  (yas-global-mode)                       ; Snippets.

  ;; Other modes that need to be activated:
  (pdf-tools-install)                     ; Internal PDF viewer

  ;; Libraries used throughout my Emacs session:
  (require 'saveplace)                    ; Saves your location in files
  (require 'vlf-setup)                    ; Deal with large files

  (add-hook 'text-mode-hook #'abbrev-mode))

(defvar pjones:first-server-frame-initialized nil
  "Non-nil when the first frame has been configured.")

(defun pjones:initialize-server-frame ()
  "Configure a new server frame."
  (unless pjones:first-server-frame-initialized
    (setq pjones:first-server-frame-initialized t)
    (desktop-read)
    (desktop-save-mode)))

;; Hook in:
(unless noninteractive
  (add-hook 'emacs-startup-hook #'pjones:boot-global-modes)
  (add-hook 'server-after-make-frame-hook #'pjones:initialize-server-frame))

;;; modes.el ends here
