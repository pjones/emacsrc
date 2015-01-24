;;; Configuration for message mode

;; Quoting library
(require 'boxquote)

;; Calendar invitations.
(require 'gnus-icalendar)
(gnus-icalendar-setup)


(defun pmade-message-mode-hook ()
  "Called to customize message mode"
  (define-key message-mode-map (kbd "s-h")       'pmade-make-html-part)
  (define-key message-mode-map (kbd "C-c C-s")   'message-kill-to-signature)
  (define-key message-mode-map (kbd "s-s")       'pmade-mail-signature-next)
  (define-key message-mode-map (kbd "C-c <tab>") 'external-abook-try-expand)
  (define-key message-mode-map (kbd "s-q")       'boxquote-region)
  (define-key message-mode-map (kbd "s-y")       'boxquote-yank))

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

(defun pmade-make-html-part ()
  "Turn the current email into HTML using Markdown."
  (interactive)
  (save-excursion
    (message-goto-body)
    (let* ((sig-point (save-excursion
                        (message-goto-signature)
                        (forward-line -1)
                        (point)))

           (subject-start (save-excursion
                            (message-goto-subject)
                            (message-beginning-of-line 1)
                            (point)))

           (subject-text (save-excursion
                           (goto-char subject-start)
                           (move-end-of-line 1)
                           (buffer-substring-no-properties
                            subject-start (point))))

           (plain-txt (buffer-substring-no-properties (point) (point-max)))

           (pandoc (concat "pandoc -sS -f markdown -t html5 -V pagetitle:"
                           (shell-quote-argument subject-text))))

      ;; Manipulate the signature so it converts to HTML.
      (save-excursion
        (goto-char sig-point)
        (let ((kill-whole-line nil)) (kill-line))
        (insert "<hr/>")
        (while (re-search-forward "^" nil t)
          (replace-match "| " nil nil)))

      (shell-command-on-region (point) (point-max) pandoc nil t)
      (insert "<#multipart type=alternative>\n")
      (insert plain-txt)
      (insert "<#part type=text/html>\n")
      (exchange-point-and-mark)
      (insert "<#/multipart>\n"))))

(defun pmade-gnus-article-hook ()
  "Called when gnus prepares to show a message"
  (visual-line-mode)
  (setq gnus-treat-mail-gravatar 'head
        gnus-treat-from-gravatar 'head
        gnus-treat-body-boundary 'head
        gnus-treat-fill-long-lines t
        gnus-treat-unsplit-urls t
        gnus-sorted-header-list '("^Newsgroups:" "^From:" "^To:" "^Cc:"
                                  "^X-URL:" "^X-Mailer:" "^Date:" "^Subject:")))

(eval-after-load "message"
  '(progn
     (add-to-list 'message-mode-hook  'pmade-message-mode-hook)
     (add-to-list 'message-setup-hook 'pmade-message-setup-hook)))

(add-hook 'gnus-article-mode-hook 'pmade-gnus-article-hook)
