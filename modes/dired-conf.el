;;; dired-conf.el -- Settings for dired-mode
(eval-when-compile
  (require 'dired))

;; Load dired-aux at runtime.
(require 'dired-aux)

(setq dired-listing-switches "-lhA --ignore='.git' --group-directories-first"
      dired-auto-revert-buffer t
      dired-isearch-filenames t
      dired-hide-details-hide-symlink-targets nil)

(defun pjones:dired-show-only-matching-files (regexp)
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun pjones:dired-remove-total-lines ()
  "Remove those useless \"total\" lines from ls."
  (let ((buffer-read-only nil))
    (save-excursion
      (goto-char (point-min))
      (flush-lines "^ *total"))))

(defun pjones:dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(defun pjones:dired-load-hook ()
  (dired-hide-details-mode) ;; Hide details by default
  (define-key dired-mode-map [?%?h] 'pjones:dired-show-only-matching-files)
  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'pjones:dired-jump-to-bottom))

(add-hook 'dired-mode-hook 'pjones:dired-load-hook)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'dired-after-readin-hook 'pjones:dired-remove-total-lines)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
