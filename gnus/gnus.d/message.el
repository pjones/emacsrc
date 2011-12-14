;;; Configuration for message mode

;; Quoting library
(require 'boxquote)

(defun pmade-message-mode-hook ()
  "Called to customize message mode"
  (define-key message-mode-map "\C-c\t"   'external-abook-try-expand)
  (define-key message-mode-map "\C-c\C-w" 'pmade-mail-signature-next)
  (define-key message-mode-map "\C-c\C-q" 'boxquote-region)
  (define-key message-mode-map "\C-c\C-y" 'boxquote-yank))

(defun pmade-message-setup-hook ()
  "Called after a new message has been initialized")

(defun pmade-mail-signature-next ()
  "Replace the current signature with the next one"
  (interactive)
  (or (boundp 'mail-signature-files)
      (setq mail-signature-files
            (directory-files (expand-file-name mail-signature-dir) t "^[A-Za-z0-9]")))
  (let ((sig-file (first mail-signature-files)))
    (setq mail-signature-files (append (cdr mail-signature-files) (list (car mail-signature-files))))
    (save-excursion
      (message-goto-signature)
      (let ((beg (point)))
        (end-of-buffer)
        (kill-region beg (point)))
      (insert-file-contents sig-file))))

(defun pmade-gnus-article-hook ()
  "Called when gnus prepares to show a message"
  (setq truncate-lines nil))

(eval-after-load "message"
  '(progn
     (add-to-list 'message-mode-hook  'pmade-message-mode-hook)
     (add-to-list 'message-setup-hook 'pmade-message-setup-hook)))

(add-hook 'gnus-article-prepare-hook 'pmade-gnus-article-hook)
