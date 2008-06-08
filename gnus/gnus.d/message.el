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
  ;; Check to see if we are replying to form mail
;;   (let ((form-mail))
;;     (save-excursion
;;       (message-goto-from)
;;       (when (looking-at "radiant@pmade.com")
;;         (setq form-mail t)))
;;     (and form-mail (pmade-reply-to-form-mail))))
  
(defun pmade-reply-to-form-mail ()
  "Reply to badly formatted form mail"
  (interactive)
  (message-goto-body)
  ;; Update the from address, taken from the body
  (let ((name) (email))
    (save-match-data
      (when (re-search-forward "Sender Name:")
        (skip-chars-forward " ")
        (let ((beg (point)))
          (move-end-of-line 1)
          (setq name (buffer-substring beg (point)))))
      (when (re-search-forward "Sender E-mail:")
        (skip-chars-forward " ")
        (let ((beg (point)))
          (move-end-of-line 1)
          (setq email (buffer-substring beg (point))))))
    (message-goto-to)
    (kill-line)
    (insert (concat name " <" email ">")))
  ;; Kill the attribution line
  (message-goto-body)
  (kill-line 2)
  ;; Clear the subject line
  (message-goto-subject)
  (save-match-data (and (re-search-forward "Re: ") (kill-line))))
      
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

(eval-after-load "message" 
  '(progn
     (add-to-list 'message-mode-hook  'pmade-message-mode-hook)
     (add-to-list 'message-setup-hook 'pmade-message-setup-hook)))
