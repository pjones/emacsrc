;;; message-conf.el --- composing mail and news messages
;;; Commentary:
;;; Code:
(require 'message)
(require 'mml)
(require 'smtpmail)

(custom-set-variables
 '(message-confirm-send t)
 '(message-send-mail-function #'smtpmail-send-it)
 '(message-directory "~/mail")
 '(message-from-style 'angles)
 '(message-citation-line-function #'message-insert-formatted-citation-line)
 '(message-citation-line-format "On %a, %b %d %Y, %N wrote:")
 '(message-cite-reply-position 'traditional)
 '(message-auto-save-directory nil)
 '(message-dont-reply-to-names '("pjones@pmade.com" "pmadeinc@gmail.com"))
 '(message-kill-buffer-on-exit t)
 '(message-signature-directory "~/notes/signatures/")
 `(mml-secure-key-preferences
   '((OpenPGP
      (sign ("pjones@devalot.com" ,epa-file-encrypt-to))
      (encrypt ("pjones@devalot.com" ,epa-file-encrypt-to))))))

;; FIXME: Use message-send-hook to convert body to HTML

;; A few extra key bindings:
(let ((map message-mode-map))
  (define-key map (kbd "C-c C-b") #'message-goto-body)
  (define-key map (kbd "C-c C-c") #'message-send-and-exit)
  (define-key map (kbd "C-c C-e") #'mml-secure-message-sign-encrypt)
  (define-key map (kbd "C-c C-f") #'message-goto-from)
  (define-key map (kbd "C-c C-h") #'pjones:message-convert-to-html)
  (define-key map (kbd "C-c C-k") #'message-kill-buffer)
  (define-key map (kbd "C-c C-s") #'message-kill-to-signature)
  (define-key map (kbd "C-c C-S-b") #'message-goto-bcc)
  (define-key map (kbd "C-c C-S-c") #'message-goto-cc)
  (define-key map (kbd "C-c C-S-s") #'message-goto-signature)
  (define-key map (kbd "C-c C-s") #'message-goto-subject)
  (define-key map (kbd "C-c C-t") #'message-goto-to))

(defun pjones:message-convert-to-html ()
  "Turn the current email into HTML using Markdown."
  (interactive)
  (save-excursion
    (message-goto-body)
    (let* ((sig-point
            (save-excursion
              (message-goto-signature)
              (forward-line -1)
              (point)))
           (subject-start
            (save-excursion
              (message-goto-subject)
              (message-beginning-of-line 1)
              (point)))
           (subject-text
            (save-excursion
              (goto-char subject-start)
              (move-end-of-line 1)
              (buffer-substring-no-properties
               subject-start (point))))
           (plain-txt (buffer-substring-no-properties (point) (point-max)))
           (pandoc (concat
                    "pandoc -f markdown -t html -s -V pagetitle:"
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

;;; message-conf.el ends here
