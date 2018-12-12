;;; modes.el -- Load files from ~/.emacs.d/pjones/modes on demand.

;; Atomically load my per-mode configuration files
(dolist (file (directory-files pjones:modes-dir t))
  (let ((basename (file-name-sans-extension (file-name-nondirectory file))))
    (when (string-match "\\(-conf\\)$" basename)
      (eval-after-load (intern (replace-match "" t t basename))
        `(load ,file)))))

;; Global minor modes:
(winner-mode)                           ; Track win conifg changes
(projectile-global-mode)                ; Project tool
(global-auto-revert-mode)               ; External changes cause a revert
(shackle-mode)                          ; Control pop-up windows.
(default-text-scale-mode)               ; Frame text scaling.
(ivy-mode)                              ; Minibuffer completion
(counsel-mode)                          ; More completion via Ivy
(which-key-mode)                        ; Remind me what keys do.
(edit-server-start)

;; Other modes that need to be activated:
(pdf-tools-install)                     ; Internal PDF viewer

;; Libraries used throughout my Emacs session:
(require 'saveplace)                    ; Saves your location in files
(require 'dired-x)                      ; Extra features for dired-mode
(require 'align)                        ; Align things

;; Defer this until after EXWM starts.
;; (desktop-save-mode)                     ; Save window/buffer configs
