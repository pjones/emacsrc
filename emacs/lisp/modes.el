;;; modes.el -- Load files from ~/.emacs.d/pjones/modes on demand.
(eval-when-compile (load "loadpath.el"))

;; Global minor modes
(winner-mode)

;; Atomically load my per-mode configuration files
(dolist (file (directory-files pjones:modes-dir t))
  (let ((basename (file-name-sans-extension (file-name-nondirectory file))))
    (when (string-match "\\(-conf\\)$" basename)
      (eval-after-load (intern (replace-match "" t t basename))
        `(load ,file)))))
