;;; dired-conf.el -- Settings for dired-mode
(eval-when-compile
  (require 'dired))

;; Load dired-aux at runtime.
(require 'dired-aux)

(setq dired-listing-switches "-lhA --ignore=.git --group-directories-first"
      dired-auto-revert-buffer t
      dired-isearch-filenames t
      dired-hide-details-hide-symlink-targets nil)

(defvar pjones:dired-keywords
  `((,(concat "\\(" (expand-file-name "~/") "\\)") ;; Replace paths to user's home dir.
     (0 (pjones:shorten-home-path))))
  "Extra things in the dired buffer to font-lock.")

(defun pjones:shorten-home-path ()
  (let ((buffer-read-only nil))
    (put-text-property (match-beginning 1) (match-end 1)
                       'display "~/"))
  nil)

(defun pjones:dired-extra-keywords ()
  "Toggle adding some extra keywords to dired buffers."
  (interactive)
  (let ((modified (buffer-modified-p)))
    (font-lock-add-keywords 'dired-mode pjones:dired-keywords t)
    (font-lock-fontify-buffer)
    (set-buffer-modified-p modified)))

(defun pjones:dired-insert-or-visit ()
  "If point is on a directory, insert that directory into the
current dired buffer.  Otherwise visit the file under point."
  (interactive)
  (let ((name (dired-get-file-for-visit)))
    (if (file-directory-p name) (dired-maybe-insert-subdir name)
      (dired-find-file))))

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

(defun pjones:dired-find-file ()
  "Use the current dired directory as a starting point for
find-file."
  (interactive)
  (let ((default-directory (dired-current-directory)))
    (ido-find-file)))

(defun pjones:dired-load-hook ()
  (dired-hide-details-mode) ;; Hide details by default
  (pjones:dired-extra-keywords)

  (let ((map dired-mode-map))
    (define-key map (kbd "C-x C-f")  'pjones:dired-find-file)
    (define-key map (kbd "C-m")      'pjones:dired-insert-or-visit)
    (define-key map (kbd "e")        'dired-toggle-read-only)
    (define-key map [?%?h]           'pjones:dired-show-only-matching-files)
    (define-key map
      (vector 'remap 'end-of-buffer) 'pjones:dired-jump-to-bottom)))

(add-hook 'dired-mode-hook 'pjones:dired-load-hook)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
(add-hook 'dired-after-readin-hook 'pjones:dired-remove-total-lines)

;; Local Variables:
;; byte-compile-warnings: (not noruntime)
;; End:
