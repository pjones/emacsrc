;;; dired-conf.el -- Settings for dired-mode
(eval-when-compile
  (require 'dired))

;; Load dired-aux at runtime.
(require 'dired-aux)

(setq dired-listing-switches "-lRA --ignore='.git' --group-directories-first"
      dired-auto-revert-buffer t
      dired-isearch-filenames t)

(defun pjones:dired-show-only-matching-files (regexp)
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun pjones:dired-load-hook ()
  (define-key dired-mode-map [?%?h] 'pjones:dired-show-only-matching-files))

(add-hook 'dired-load-hook 'pjones:dired-load-hook)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
